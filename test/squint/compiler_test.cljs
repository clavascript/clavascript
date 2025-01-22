(ns squint.compiler-test
  (:require
   [clojure.string :as str]
   [clojure.test :as t :refer [async deftest is testing]]
   [squint.compiler :as squint]
   [squint.jsx-test]
   [squint.html-test]
   [squint.string-test]
   [squint.nrepl-test]
   [squint.test-utils :refer [eq js! jss! jsv!]]
   ["fs" :as fs]
   ["child_process" :as process]
   ["node:util" :as util]
   [promesa.core :as p]))

(deftest return-test
  (is (str/includes? (jss! '(do (def x (do 1 2 nil))))
                     "return"))
  (is (str/includes? (jss! '(do (def x (do 1 2 "foo"))))
                     "return"))
  (is (str/includes? (jss! '(do (def x (do 1 2 :foo))))
                     "return"))
  (is (str/includes? (jss! "(do (def x (do 1 2 \"hello\")))")
                     "return"))
  (let [s (jss! "(do (def x (do 1 2 [1 2 3])) x)")]
    (is (eq #js [1 2 3] (js/eval s))))
  (let [s (jss! "(do (def x (do 1 2 {:x 1 :y 2})) x)")]
    (is (eq #js {:x 1 :y 2} (js/eval s))))
  (let [s (jss! "(do (def x (do 1 2 #js {:x 1 :y 2})) x)")]
    (is (eq (str #js {:x 1 :y 2}) (str (js/eval s))))))

(deftest do-test
  (let [[v s] (js! '(do 1 2 3))]
    (is (= 3 v))
    (is (not (str/includes? s "function"))))
  (let [[v s] (js! '(do 1 2 3 (do 4 5 6)))]
    (is (= 6 v))
    (is (not (str/includes? s "function"))))
  (let [[v s] (js! '(do (def x (do 4 5 6))
                        x))]
    (is (= 6 v))
    (is (str/includes? s "() =>")))
  (let [[v s] (js! '(let [x (do 4 5 6)]
                      x))]
    (is (= 6 v))
    (is (str/includes? s "() => "))))

(deftest let-test
  (is (= 3 (jsv! '(let [x (do 1 2 3)] x))))
  (is (= 3 (jsv! '(let [x 1 x (+ x 2)] x))))
  (let [s (jss! '(let [x 1 x (let [x (+ x 1)]
                               x)] x))]
    (is (= 2 (js/eval s))))
  (is (= 7 (jsv! '(let [{:keys [a b]} {:a 1 :b (+ 1 2 3)}]
                    (+ a b)))))
  (is (= 8 (jsv!
            '(+ 1
                (let [{:keys [a b]} {:a 1 :b (+ 1 2 3)}]
                  (+ a b)))))))

(deftest let-interop-test
  (is (= "f" (jsv! '(let [x "foo"]
                      (.substring x 0 1)))))
  (is (= 3 (jsv! '(let [x "foo"]
                    (.-length x))))))

(deftest let-shadow-test
  (is (= 1 (jsv! '(let [name 1]
                    name))))
  (is (= 1 (jsv! '(let [name (fn [] 1)]
                    (name)))))
  (let [s (jss! '(let [name (fn [_] 1)]
                   (vec (map name [1 2 3]))))]
    (is (eq #js [1 1 1]
            (js/eval s))))
  (let [s (jss! '(let [name (fn [_] 1)
                       name (fn [_] 2)]
                   (vec (map name [1 2 3]))))]
    (is (eq #js [2 2 2]
            (js/eval s)))))

(deftest destructure-test
  (let [s (jss! "(let [^js {:keys [a b c]} #js {:a 1 :b 2 :c 3}]
                   (+ a b c))")]
    (is (= 6 (js/eval s))))
  (is (eq #js [1 #js [2 3]]
          (jsv! "(let [[x & xs] [1 2 3]]
                   [x xs])"))))

(deftest fn-test
  (let [s (jss! '(let [f (fn [x] x)]
                   f))]
    (is (= 1 ((js/eval s) 1))))
  (let [s (jss! '(let [f (fn [x] 1 2 x)]
                   f))]
    (is (= 1 ((js/eval s) 1))))
  (let [s (jss! '(let [f (fn [x] 1 2 (+ 1 x))]
                   f))]
    (is (= 2 ((js/eval s) 1))))
  (let [s (jss! '(let [f (fn [x] 1 2 (do 1 x))]
                   f))]
    (is (= 1 ((js/eval s) 1))))
  (is (= 1 (jsv! '(do (defn foo [] (fn [x] x)) ((foo) 1))))))

(deftest jss!-test
  (is (not (str/includes? (jss! '(def x 1) {:repl false}) "globalThis")))
  (is (str/includes? (jss! '(def x 1) {:repl true}) "globalThis")))

(deftest fn-varargs-test
  (doseq [repl [true false]]
    (testing "vararg fixed arity"
      (is (nil? (jsv! '(let [f (fn foo [x y & zs] zs)] (f 1 2)) {:repl repl}))))
    (testing "vararg vararg arity"
      (is (eq #js [3 4] (jsv! '(let [f (fn foo [x y & zs] zs)] (f 1 2 3 4)) {:repl repl}))))
    (testing "multi vararg fixed arity"
      (is (eq 1 (jsv! '(let [f (fn foo
                                 ([y] 1)
                                 ([y & zs] zs))]
                         (f 1))
                      {:repl repl}))))
    (testing "multi vararg vararg arity"
      (is (eq [2] (jsv! '(let [f (fn foo
                                   ([y] 1)
                                   ([y & zs] zs))]
                           (f 1 2)) {:repl repl}))))))

(deftest fn-multi-arity-test
  (is (= 1 (jsv! '(let [f (fn foo ([x] x) ([x y] y))] (f 1)))))
  (is (= 2 (jsv! '(let [f (fn foo ([x] x) ([x y] y))] (f 1 2))))))

(deftest fn-multi-varargs-test
  (is (= 1 (jsv! '(let [f (fn foo ([x] x) ([x y & zs] zs))] (f 1)))))
  (is (eq '(3 4) (jsv! '(let [f (fn foo ([x] x) ([x y & zs] zs))] (f 1 2 3 4)))))
  (is (nil? (jsv! '(let [f (fn foo ([x] x) ([x y & zs] zs))] (f 1 2))))))

(deftest defn-test
  (let [s (jss! '(do (defn f [x] x) f))]
    (is (= 1 ((js/eval s) 1))))
  (let [s (jss! '(do (defn f [x] (let [y 1] (+ x y))) f))]
    (is (= 2 ((js/eval s) 1))))
  (let [s (jss! '(do (defn foo [x]
                       (dissoc! x :foo))
                     (foo {:a 1 :foo :bar})))]
    (is (eq {:a 1} (js/eval s))))
  (let [s (jss! "(do (defn f [^js {:keys [a b c]}] (+ a b c)) f)")]
    (is (= 6 ((js/eval s) #js {:a 1 :b 2 :c 3}))))
  (let [s (jss! '(do (defn quux [x]
                       (if (= 1 1)
                         1
                         2))
                     (quux 1)))]
    (is (= 1 (js/eval s)))))

(deftest defn-multi-arity-test
  (is (= 1 (jsv! '(do
                    (defn foo ([x] x) ([x y] y))
                    (foo 1)))))
  (is (= 2 (jsv! '(do
                    (defn foo ([x] 1) ([x y] y))
                    (foo 1 2))))))

(deftest defn-recur-test
  (let [s (jss! '(do (defn quux [x]
                       (if (> x 0)
                         (recur (dec x))
                         x))
                     (quux 1)))]
    (is (zero? (js/eval s)))))

(deftest defn-varargs-test
  (let [s (jss! '(do (defn foo [x & args] args) (foo 1 2 3)))]
    (is (eq '(2 3) (js/eval s)))))

(deftest defn-multi-varargs-test
  (is (eq [1 [1 2 '(3 4)]]
          (js/eval
           (jss! '(do (defn foo
                        ([x] x)
                        ([x y & args]
                         [x y args]))
                      [(foo 1) (foo 1 2 3 4)]))))))

(deftest loop-test
  (let [s (jss! '(loop [x 1] (+ 1 2 x)))]
    (is (= 4 (js/eval s))))
  (let [s (jss! '(loop [x 10]
                   (if (> x 0)
                     (recur (dec x))
                     x)))]
    (is (zero? (js/eval s)))))

(deftest if-test
  (is (true? (jsv! "(if 0 true false)")))
  (let [s (jss! "[(if false true false)]")]
    (is (false? (first (js/eval s)))))
  (let [s (jss! "(let [x (if (inc 1) (inc 2) (inc 3))]
                   x)")]
    (is (= 3 (js/eval s))))
  (let [s (jss! "(let [x (do 1 (if (inc 1) (inc 2) (inc 3)))]
                   x)")]
    (is (= 3 (js/eval s))))
  (is (eq #js {:a 1} (jsv! "{:a (or 1 (cond true (prn :yes)) 2)}"))))

(deftest zero?-test
  (is (str/includes? (jss! "(if (zero? x) 1 2)") "== 0"))
  (is (not (str/includes? (jss! "(if (zero? x) 1 2)") "truth_"))))

(deftest no-truth-check-test
  (let [inputs ["(if (zero? 0) 1 2)" "(when (< 1 2) 1)" "(when (= 1 1) 1)"
                "(let [x (zero? 0)] (when x 1))"
                "(if (neg? 1) 0 1)" "(if (not 1) 0 1)"
                "(if \"foo\" 1 2)" "(if :foo 1 2)"
                "(let [x nil] (if (nil? x) 1 2))"
                "(let [x (zero? 0) y x] (if y 1 2))"
                "(if (coercive-boolean (range 0 1)) 1 2)"]]
    (doseq [input inputs]
      (let [js (jss! input)]
        (is (not (str/includes? js "truth_")) (str "contains truth check: " input "\n" js))
        (is (eq 1 (js/eval js)))))))

(deftest doseq-test
  (let [s (jss! '(let [a []]
                   (doseq [x [1 2 3]]
                     (.push a x))
                   a))]
    (is (eq [1 2 3] (js/eval s))))
  (let [s (jss! '(let [a []]
                   (doseq [x [1 2 3]
                           y [4 5 6]]
                     (.push a [x y]))
                   a))]
    (is (eq [[1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6]]
            (js/eval s))))
  (let [s (jss! '(let [a []]
                   (doseq [x [1 2 3 4 5 6]
                           :when (even? x)]
                     (.push a x))
                   a))]
    (is (eq [2 4 6]
            (js/eval s))))
  (let [s (jss! '(let [a []]
                   (doseq [x [1 2 3]
                           y [4 5 6]
                           :when (< (+ x y) 8)]
                     (.push a [x y]))
                   a))]
    (is (eq [[1 4] [1 5] [1 6] [2 4] [2 5] [3 4]]
            (js/eval s))))
  (let [s (jss! '(let [a []]
                   (doseq [x [1 2 3]
                           y [4 5 6]
                           :let [z (+ x y)]]
                     (.push a z))
                   a))]
    (is (eq [5 6 7 6 7 8 7 8 9]
            (js/eval s))))
  (let [s (jss! '(let [a []]
                   (doseq [x (range 3) y (range 3) :while (not= x y)]
                     (.push a [x y]))
                   a))]
    (is (eq [[1 0] [2 0] [2 1]]
            (js/eval s))))
  ;; https://clojuredocs.org/clojure.core/for#example-542692d3c026201cdc326fa9
  (testing ":while placement"
    (let [s (jss! '(let [a []]
                     (doseq [x [1 2 3]
                             y [1 2 3]
                             :while (<= x y)
                             z [1 2 3]]
                       (.push a [x y z]))
                     a))]
      (is (eq [[1 1 1] [1 1 2] [1 1 3]
               [1 2 1] [1 2 2] [1 2 3]
               [1 3 1] [1 3 2] [1 3 3]]
              (js/eval s))))
    (let [s (jss! '(let [a []]
                     (doseq [x [1 2 3]
                             y [1 2 3]
                             z [1 2 3]
                             :while (<= x y)]
                       (.push a [x y z]))
                     a))]
      (is (eq [[1 1 1] [1 1 2] [1 1 3]
               [1 2 1] [1 2 2] [1 2 3]
               [1 3 1] [1 3 2] [1 3 3]
               [2 2 1] [2 2 2] [2 2 3]
               [2 3 1] [2 3 2] [2 3 3]
               [3 3 1] [3 3 2] [3 3 3]]
              (js/eval s)))))
  (testing "return position in function"
    (let [f (jsv! '(do (defn foo [x a] (doseq [i x] (swap! a conj i))) foo))]
      (is f)
      (let [a (jsv! '(atom []))
            d (jsv! 'deref)]
        (f [1 2 3] a)
        (is (eq #js [1 2 3] (d a)))))
    (let [f (jsv! '(do (def a {}) (doseq [i [1 2 3]] (set! (.-foo a) (+ (or (.-foo a) 0) i))) a))]
      (is (= 6 (aget f "foo")))))
  (testing "iterate over object"
    (let [r (jsv! '(do
                     (def a (atom []))
                     (doseq [[k v] {:a 1 :b 2}]
                       (swap! a conj [k v]))
                     @a))]
      (is (eq #js [#js ["a" 1] #js ["b" 2]] r))))
  (testing "clash with core var"
    (is (eq #js [#js ["a" 1] #js ["b" 2]]
            (jsv! '(do
                     (def a (atom []))
                     (doseq [_ {:a 1 :b 2}]
                       (swap! a conj _))
                     @a))))))

;; TODO:
(deftest for-test
  (let [s (jss! '(vec (for [x [1 2 3] y [4 5 6]] [x y])))]
    (is (eq '([1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6])
            (js/eval s))))
  (let [s (jss! '(vec (for [x [1 2 3 4 5 6] :when (even? x)] x)))]
    (is (eq '[2 4 6] (js/eval s))))
  (let [s (jss! '(vec (for [x [1 2 3]
                            y [4 5 6]
                            :when (< (+ x y) 8)]
                        [x y])))]
    (is (eq [[1 4] [1 5] [1 6] [2 4] [2 5] [3 4]]
            (js/eval s))))
  (let [s (jss! '(vec (for [x (range 3) y (range 3) :while (not= x y)]
                        [x y])))]
    (is (eq [[1 0] [2 0] [2 1]]
            (js/eval s))))
  ;; https://clojuredocs.org/clojure.core/for#example-542692d3c026201cdc326fa9
  (testing ":while placement"
    (let [s (jss! '(vec
                    (for [x [1 2 3]
                          y [1 2 3]
                          :while (<= x y)
                          z [1 2 3]]
                      [x y z])))]
      (is (eq [[1 1 1] [1 1 2] [1 1 3]
               [1 2 1] [1 2 2] [1 2 3]
               [1 3 1] [1 3 2] [1 3 3]]
              (js/eval s))))
    (let [s (jss! '(vec
                    (for [x [1 2 3]
                          y [1 2 3]
                          z [1 2 3]
                          :while (<= x y)]
                      [x y z])))]
      (is (eq [[1 1 1] [1 1 2] [1 1 3]
               [1 2 1] [1 2 2] [1 2 3]
               [1 3 1] [1 3 2] [1 3 3]
               [2 2 1] [2 2 2] [2 2 3]
               [2 3 1] [2 3 2] [2 3 3]
               [3 3 1] [3 3 2] [3 3 3]]
              (js/eval s)))))
  (let [s (jss! '(vec (for [x (range 3) y (range 3)
                            :let [z (+ x y)]]
                        z)))]
    (is (eq [0 1 2 1 2 3 2 3 4]
            (js/eval s))))
  (let [s (jss! '(vec (for [x (range 3)
                            :let [x' (inc x)]
                            y (range 3)
                            :let [y' (inc y)
                                  z (+ x' y')]]
                        (let [z' (inc z)]
                          (inc z')))))]
    (is (eq [4 5 6 5 6 7 6 7 8]
            (js/eval s))))
  (testing "return position in function"
    (let [f (jsv! '(do (defn foo [x] (for [i x] i)) foo))]
      (is f)
      (is (= [1 2 3] (vec (f [1 2 3]))))))
  (testing "iterate over object"
    (let [r (jsv! '(vec (for [[k v] {:a 1 :b 2}]
                          [k v])))]
      (is (eq #js [#js ["a" 1] #js ["b" 2]] r))))
  (testing "clash with core var"
    (let [r (jsv! '(vec (for [_ {:a 1 :b 2}]
                          _)))]
      (is (eq #js [#js ["a" 1] #js ["b" 2]] r))))
  (testing "repl-mode"
    (let [s (jss! "(vec (for [x [1 2 3]]
                           x))"
                  {:repl true})]
      (is (str/includes? s "globalThis"))
      (is (eq [1 2 3] (js/eval s))))))

(deftest Math-test
  (let [expr '(Math/sqrt 3.14)]
    (is (eq (Math/sqrt 3.14) (jsv! (str expr))))
    (testing "repl-mode"
      (let [s (jss! (str expr) {:repl true})]
        (is (str/includes? s "globalThis.user"))
        (is (not (str/includes? s "globalThis.user.Math")))
        (is (eq (Math/sqrt 3.14) (js/eval s)))))))

(deftest regex-test
  (is (eq '("foo")
          (jsv! '(.match "foo foo" #"foo")))))

(deftest new-test
  (is (eq "hello" (jsv! '(str (js/String. "hello"))))))

(deftest quote-test
  (is (eq '{x 1} (jsv! (list 'quote '{x 1})))))

(deftest case-test
  (is (= 2 (jsv! '(case 1 1 2 3 4))))
  (is (= 5 (jsv! '(case 6 1 2 3 4 (inc 4)))))
  (is (= 2 (jsv! '(case 1 :foo :bar 1 2))))
  (is (= "bar" (jsv! '(case :foo :foo :bar))))
  (let [s (jss! '(let [x (case 1 1 2 3 4)]
                   (inc x)))]
    (is (= 3 (js/eval s))))
  (let [s (jss! '(do (defn foo []
                       (case 1 1 2 3 4))
                     (foo)))]
    (is (= 2 (js/eval s)))))

(deftest dot-test
  (let [s (jss! "(do (def x (.-x #js {:x 1})) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (. #js {:x 1} -x)) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (.x #js {:x (fn [] 1)})) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (.x #js {:x (fn [x] x)} 1)) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (. #js {:x (fn [x] x)} x 1)) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(do (def x (. #js {:x (fn [x] x)} (x 1))) x)")]
    (is (= 1 (js/eval s))))
  (let [s (jss! "(.goto #js {:goto (fn [x] [:hello x])} 10)")]
    (is (eq [:hello 10] (js/eval s)))))

(deftest dotdot-test
  (let [s (jss! "(.. #js {:foo #js {:bar 2}} -foo -bar)")]
    (is (= 2 (js/eval s)))))

#_(js-delete js/require.cache (js/require.resolve "/tmp/debug.js"))
#_(js/require "/tmp/debug.js")

#_(deftest backtick-test
    (is (= '(assoc {} :foo :bar) (jsv! "`(assoc {} :foo :bar)"))))

#_(deftest munged-core-name-test
    (is (jsv! '(boolean 1))))


(deftest defprotocol-extend-type-string-test
  (is (eq "foo" (jsv! '(do (defprotocol IFoo (foo [_] "docstring"))
                           (extend-type string IFoo (foo [_] :foo)) (foo "bar"))))))

(deftest defprotocol-extend-protocol-test
  (is (eq ["dudedude" 1]
          (jsv! '(do (defprotocol Identity (i [this]))
                     (extend-protocol Identity string (i [s] (str s s))
                                      object (i [s] s))
                     [(i "dude") (i 1)])))))

(deftest defprotocol-extend-protocol-empty-body-test
  (is (eq nil
          (jsv! '(do (defprotocol Identity (i [this]))
                     (extend-protocol Identity string (i [s]))
                     (i "dude"))))))

(deftest defprotocol-extend-protocol-nil
  (is (eq ["nil" "boolean" "string" true]
          (jsv! '(do (defprotocol Identity (i [this]))
                     (extend-protocol Identity nil (i [s] "nil"))
                     (extend-protocol Identity boolean (i [s] "boolean"))
                     (extend-protocol Identity string (i [s] "string"))
                     [(i nil) (i false) (i "")
                      (satisfies? Identity nil)
                      ])))))

(deftest deftype-test
  (is (= 1 (jsv! '(do (deftype Foo [x]) (.-x (->Foo 1))))))
  (is (eq [:foo :bar]
          (jsv! '(do
                   (defprotocol IFoo (foo [_]) (bar [_]))
                   (deftype Foo [x] IFoo (foo [_] :foo)
                            (bar [_] :bar))
                   (let [x (->Foo 1)]
                     [(foo x) (bar x)])))))
  (is (eq [:foo 2]
          (jsv! '(do (defprotocol IFoo (foo [_]) (bar [_]))
                     (deftype Foo [^:mutable x]
                       IFoo
                       (foo [_] [:foo x])
                       (bar [_] :bar))
                     (def x  (->Foo 1))
                     (set! (.-x x) 2)
                     (foo x)))))
  (is (eq [:bar 1 2]
          (jsv! '(do (defprotocol IFoo (foo [_]) (bar [_] [_ a b]))
                     (deftype Foo []
                       IFoo
                       (foo [_] :foo)
                       (bar [_] [:bar])
                       (bar [_ a b] [:bar a b]))
                     (bar (->Foo) 1 2))))))

(deftest satisfies?-test
  #_#_(is (jsv! '(do (defprotocol IFoo)
                     (satisfies? (reify IFoo)))))
  (is (jsv! '(do (defprotocol IFoo (-foo [_]))
                 (satisfies? (reify IFoo
                               (-foo [_] "bar"))))))
  (is (jsv! '(do (defprotocol IFoo)
                 (deftype Foo [] IFoo)
                 (satisfies? IFoo (->Foo)))))
  (is (jsv! '(do (defprotocol IFoo (-foo [_]))
                 (deftype Foo [] IFoo (-foo [_] "bar"))
                 (satisfies? IFoo (->Foo)))))
  (is (jsv! '(do (defprotocol IFoo)
                 (extend-type number IFoo)
                 (satisfies? IFoo 1))))
  (is (jsv! '(do (defprotocol IFoo)
                 (extend-type string IFoo)
                 (satisfies? IFoo "bar")))))

(deftest set-test
  (is (eq (js/Set. #js [1 2 3]) (jsv! #{1 2 3})))
  (is (eq (js/Set. [1 2 3]) (jsv! '(set [1 2 3]))))
  (is (eq (js/Set. [#js ["a" 1] #js ["b" 2]])
          (jsv! '(set {:a 1 :b 2}))))
  (is (eq (js/Set. [#js ["a" 1] #js ["b" 2]])
          (jsv! '(set (js/Map. [[:a 1] [:b 2]])))))
  (is (eq (js/Set.) (jsv! '(set))))
  (is (eq (js/Set.) (jsv! '(set nil)))))

(deftest await-test
  (async done
    (->
     (.then (jsv! '(do (defn ^:async foo []
                         (js-await (js/Promise.resolve :hello)))

                       (defn ^:async bar []
                         (let [x (js-await (foo))]
                           x))

                       (bar)))
            (fn [v]
              (is (= "hello" v))))
     (.catch (fn [err]
               (is false (.-message err))))
     (.finally #(done)))))

(deftest top-level-await-test
  (is (str/includes? (jss! "(js-await 1)") "await")))

(deftest await-variadic-test
  (async done
    (->
     (.then (jsv! '(do (defn ^:async foo [& xs] (js-await 10))
                       (defn ^:async bar [x & xs] (js-await 20))
                       (defn ^:async baz
                         ([x] (baz x 1 2 3))
                         ([x & xs]
                          (let [x (js/await (foo x))
                                y (js/await (apply bar xs))]
                            (+ x y))))

                       (baz 1)))
            (fn [v]
              (is (= 30 v))))
     (.catch (fn [err]
               (is false (.-message err))))
     (.finally #(done)))))

(deftest async-await-anon-fn-test
  (is (instance? js/Promise (jsv! "((fn ^:async foo [] (js-await {})))")))
  (is (instance? js/Promise (jsv! "((^:async fn [] (js-await {})))"))))

(deftest native-js-array-test
  (let [s (jss! "(let [x 2
                       x #js [1 2 x]]
                   x)")
        x (js/eval s)]
    (is (array? x))
    (is (= [1 2 2] (js->clj x))))
  (is (= 1 (jsv! "(aget  #js [1 2 3] 0)"))))

(deftest keyword-call-test
  (is (= "bar" (jsv! '(:foo {:foo :bar}))))
  (is (= "bar" (jsv! '(:foo nil :bar))))
  (is (= "bar" (jsv! '(let [{:keys [foo] :or {foo :bar}} nil]
                        foo)))))

(deftest coll-call-test
  (is (= "bar" (jsv! '({:foo :bar} :foo))))
  (is (= "the-default" (jsv! '({:foo :bar} :default :the-default))))
  (is (= "foo" (jsv! '(#{:foo :bar} :foo))))
  (is (= "the-default" (jsv! '(#{:foo :bar} :dude :the-default)))))

(deftest minus-single-arg-test
  (is (= -10 (jsv! '(- 10))))
  (is (= -11 (jsv! '(- 10 21)))))

(deftest namespace-keywords
  (is (eq #js {"foo/bar" "baz"} (jsv! {:foo/bar :baz})))
  (is (eq "hello/world" (jsv! "(ns hello) ::world"))))

(deftest pr-str-test
  (is (eq (js/Set. #js ["a" "b" "c"]) (js/Set. (js/JSON.parse (jsv! '(pr-str #{:a :b :c})))))))

(deftest str-test
  (is (eq "123" (jsv! '(str 1 2 3))))
  (is (eq "foobarbaz", (jsv! '(str "foo" "bar" "baz"))))
  (is (eq "1barfirst,second[object Object]"
          (jsv! '(str 1 "bar" [:first :second] {"hello" "goodbye"})))))

(deftest comp-test
  (is (eq "0" (jsv! '((comp) "0")))
      "0-arity is identity")
  (is (eq 4 (jsv! '((comp inc) 3)))
      "1-arity")
  (is (eq "0123" (jsv! '((comp #(str % "3") #(str % "2") #(str % "1"))
                         "0")))
      "order"))

(deftest conj-test
  (testing "corner cases"
    (is (eq [], (jsv! '(conj))))
    (is (= true, (jsv! '(vector? (conj)))))
    (is (eq '(), (jsv! '(conj nil))))
    (is (= true, (jsv! '(array? (conj nil)))))
    (is (eq [2 1] (jsv! '(conj nil 1 2)))))
  (testing "arrays"
    (is (eq [1 2 3 4] (jsv! '(conj [1 2 3 4]))))
    (is (eq [1 2 3 4] (jsv! '(conj [1 2 3] 4))))
    (is (eq [1 2 3 4] (jsv! '(conj [1 2] 3 4)))))
  (testing "lists"
    (is (eq '(1 2 3 4) (jsv! '(conj '(1 2 3 4)))))
    (is (eq '(1 2 3 4) (jsv! '(conj '(2 3 4) 1))))
    (is (eq '(1 2 3 4) (jsv! '(conj '(3 4) 2 1)))))
  (testing "sets"
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(conj #{1 2 3 4}))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(conj #{1 2 3} 4))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(conj #{1 2} 3 4)))))
  (testing "objects"
    (is (eq #js {:a "b" :c "d"} (jsv! '(conj {:a "b" :c "d"}))))
    (is (eq #js {:a "b" :c "d" :e "f"} (jsv! '(conj {:a "b" :c "d"} {:e "f"}))))
    (is (eq #js {"1" 2 "3" 4} (jsv! '(conj {"1" 2} ["3" 4]))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(conj {"1" 2} ["3" 4] ["5" 6])))))
  (testing "maps"
    (is (eq (js/Map. #js [#js ["a" "b"] #js ["c" "d"]])
            (jsv! '(conj (js/Map. [["a" "b"] ["c" "d"]])))))
    (is (eq (js/Map. #js [#js ["a" "b"] #js ["c" "d"]])
            (jsv! '(conj (js/Map. [["a" "b"]]) {:c "d"}))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(conj (js/Map. [[1 2]]) [3 4]))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(conj (js/Map. [[1 2]]) [3 4] [5 6])))))
  (testing "lazy iterable"
    (is (eq [0 1 2 3 4 5]
            (vec (jsv! '(conj (map inc [4])
                              0 1 2 3 4))))))
  (testing "other types"
    (is (thrown? js/Error (jsv! '(conj "foo"))))))

(deftest conj!-test
  (testing "corner cases"
    (is (eq [], (jsv! '(conj!))))
    (is (eq '(), (jsv! '(conj! nil))))
    (is (eq [1 2], (jsv! '(conj! nil 1 2)))))
  (testing "arrays"
    (is (eq [1 2 3 4] (jsv! '(conj! [1 2 3 4]))))
    (is (eq [1 2 3 4] (jsv! '(conj! [1 2 3] 4))))
    (is (eq [1 2 3 4] (jsv! '(let [x [1 2 3]]
                               (conj! x 4)
                               x))))
    (is (eq [1 2 3 4] (jsv! '(conj! [1 2] 3 4))))
    (is (eq [1 2 3 4] (jsv! '(let [x [1 2]]
                               (conj! x 3 4)
                               x)))))
  (testing "lists"
    (is (eq '(1 2 3 4) (jsv! '(conj '(1 2 3 4)))))
    (is (eq '(1 2 3 4) (jsv! '(conj '(2 3 4) 1))))
    (is (eq '(1 2 3 4) (jsv! '(let [x '(2 3 4)]
                                (conj! x 1)
                                x))))
    (is (eq '(1 2 3 4) (jsv! '(conj! '(3 4) 2 1))))
    (is (eq '(1 2 3 4) (jsv! '(let [x '(3 4)]
                                (conj! x 2 1)
                                x)))))
  (testing "sets"
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(conj! #{1 2 3 4}))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(conj! #{1 2 3} 4))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(let [x #{1 2 3}]
                                             (conj! x 4)
                                             x))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(conj! #{1 2} 3 4))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(let [x #{1 2}]
                                             (conj! x 3 4)
                                             x)))))
  (testing "objects"
    (is (eq #js {:a "b" :c "d"} (jsv! '(conj! {:a "b" :c "d"}))))
    (is (eq #js {"1" 2 "3" 4} (jsv! '(conj! {"1" 2} ["3" 4]))))
    (is (eq #js {"1" 2 "3" 4} (jsv! '(let [x {"1" 2}]
                                       (conj! x ["3" 4])
                                       x))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(conj! {"1" 2} ["3" 4] ["5" 6]))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(let [x {"1" 2}]
                                             (conj! x ["3" 4] ["5" 6])
                                             x)))))
  (testing "maps"
    (is (eq (js/Map. #js [#js ["a" "b"] #js ["c" "d"]])
            (jsv! '(conj! (js/Map. [["a" "b"] ["c" "d"]])))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(conj! (js/Map. [[1 2]]) [3 4]))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(let [x (js/Map. [[1 2]])]
                     (conj! x [3 4])))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(conj! (js/Map. [[1 2]]) [3 4] [5 6]))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(let [x (js/Map. [[1 2]])]
                     (conj! x [3 4] [5 6])
                     x)))))
  (testing "other types"
    (is (thrown? js/Error (jsv! '(conj! "foo"))))))

(deftest contains?-test
  (testing "corner cases"
    (is (= false (jsv! '(contains? nil nil))))
    (is (= false (jsv! '(contains? 1 nil))))
    (is (= false (jsv! '(contains? 1 1))))
    (is (= false (jsv! '(contains? "foo" "foo"))))
    (is (= false (jsv! '(contains? "foo" nil)))))
  (testing "arrays"
    (is (= true (jsv! '(contains? [1 2 3] 0))))
    (is (= true (jsv! '(contains? [1 2 3] 1))))
    (is (= true (jsv! '(contains? [1 2 3] 2))))
    (is (= false (jsv! '(contains? [1 2 3] 3)))))
  (testing "sets"
    (is (= false (jsv! '(contains? #{1 2 3} 0))))
    (is (= true (jsv! '(contains? #{1 2 3} 1))))
    (is (= true (jsv! '(contains? #{1 2 3} 2))))
    (is (= true (jsv! '(contains? #{1 2 3} 3))))
    (is (= true (jsv! '(contains? #{1 2 3 nil} nil)))))
  (testing "objects"
    (is (= true (jsv! '(contains? {:a 1} :a))))
    (is (= false (jsv! '(contains? {:a 1} :b)))))
  (testing "maps"
    (is (= true (jsv! '(contains? (js/Map. [[:a 1]]) :a))))
    (is (= false (jsv! '(contains? (js/Map. [[:a 1]]) :b))))))

(deftest assoc-test
  (testing "arrays"
    (is (eq [1 2 8 4] (jsv! '(assoc [1 2 3 4] 2 8))))
    (is (eq [6 2 8 4] (jsv! '(assoc [1 2 3 4] 2 8 0 6)))))
  (testing "objects"
    (is (eq #js {"1" 2 "3" 4} (jsv! '(assoc {"1" 2} "3" 4))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(assoc {"1" 2} "3" 4 "5" 6)))))
  (testing "maps"
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(assoc (js/Map. [[1 2]]) 3 4))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(assoc (js/Map. [[1 2]]) 3 4 5 6)))))
  (testing "other types"
    (is (thrown? js/Error (jsv! '(assoc! "foo" 1 2))))))

(deftest assoc!-test
  (testing "arrays"
    (is (eq [1 2 8 4] (jsv! '(assoc! [1 2 3 4] 2 8))))
    (is (eq [1 2 8 4] (jsv! '(let [x [1 2 3 4]]
                               (assoc! x 2 8)
                               x))))
    (is (eq [6 2 8 4] (jsv! '(assoc! [1 2 3 4] 2 8 0 6))))
    (is (eq [6 2 8 4] (jsv! '(let [x [1 2 3 4]]
                               (assoc! x 2 8 0 6)
                               x)))))
  (testing "objects"
    (is (eq #js {"1" 2 "3" 4} (jsv! '(assoc! {"1" 2} "3" 4))))
    (is (eq #js {"1" 2 "3" 4} (jsv! '(let [x {"1" 2}]
                                       (assoc! x "3" 4)
                                       x))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(assoc! {"1" 2} "3" 4 "5" 6))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(let [x {"1" 2}]
                                             (assoc! x "3" 4 "5" 6)
                                             x)))))
  (testing "maps"
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(assoc! (js/Map. [[1 2]]) 3 4))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(let [x (js/Map. [[1 2]])]
                     (assoc! x 3 4)
                     x))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(assoc! (js/Map. [[1 2]]) 3 4 5 6))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(let [x (js/Map. [[1 2]])]
                     (assoc! x 3 4 5 6)
                     x)))))
  (testing "other types"
    (is (thrown? js/Error (jsv! '(assoc! "foo" 1 2))))
    (is (eq 1 (jsv! '(get (doto (js/eval "class Foo { }; new Foo()")
                            (assoc! :foo 1)) :foo))))))

(deftest assoc-in-test
  (testing "happy path"
    (is (eq #js {"1" 3}
            (jsv! '(assoc-in {"1" 2} ["1"] 3))))
    (is (eq #js {"1" #js [(js/Map. #js [#js [8 9]])]}
            (jsv! '(assoc-in {"1" [(js/Map. [[8 5]])]}
                             ["1" 0 8]
                             9))))
    (is (eq true (jsv! '(let [x (js/Map. [[8 5]])
                              y [x]
                              z {"1" y}
                              z* (assoc-in z ["1" 0 8] 9)
                              y* (get z* "1")
                              x* (get y* 0)]
                          (and (not= x y*)
                               (not= y y*)
                               (not= z z*))))))
    (is (eq {:foo {:bar :baz}} (jsv! (assoc-in {} [:foo :bar] :baz)))))
  (testing "invalid data in path"
    (is (thrown? js/Error (jsv! '(assoc-in "foo" [0] 2))))
    (is (eq #js {"0" #js {"1" 2}, "1" "foo"} (jsv! '(assoc-in {"1" "foo"} [0 1] 2))))))

(deftest assoc-in!-test
  (testing "happy path"
    (is (eq #js {"1" 3}
            (jsv! '(let [x {"1" 2}]
                     (assoc-in! x ["1"] 3)
                     x))))
    (is (eq true (jsv! '(let [x (js/Map. [[8 5]])
                              y [x]
                              z {"1" y}
                              z* z]
                          (assoc-in! z ["1" 0 8] 9)
                          (and (= x (get (get z "1") 0))
                               (= y (get z "1"))
                               (= z z*)))))))
  (is (eq #js {"0" #js {"1" 2}, "1" "foo"} (jsv! '(assoc-in! {"1" "foo"} [0 1] 2))))
  (testing "invalid data in path"
    (is (thrown? js/Error (jsv! '(assoc-in! "foo" [0] 2)))))
  (testing "immutable in the middle"
    (is (eq #js {:a #js {:b 3}} (jsv! '(assoc-in! (doto {:a {:b 2}} (js/Object.freeze)) [:a :b] 3))))))

(deftest get-test
  (testing "corner cases"
    (is (= nil (jsv! '(get nil nil))))
    (is (= nil (jsv! '(get nil 0))))
    (is (= nil (jsv! '(get 1 nil))))
    (is (= nil (jsv! '(get "1" nil))))
    (is (= nil (jsv! '(get true nil))))
    (is (= nil (jsv! '(get :foo nil)))))
  (testing "maps"
    (is (eq nil (jsv! '(get (js/Map. [["my-key" 1]]) nil))))
    (is (eq 1 (jsv! '(get (js/Map. [["my-key" 1]]) "my-key"))))
    (is (identical? js/undefined (jsv! '(get (js/Map. [["my-key" 1]]) "bad-key"))))
    (is (eq 3 (jsv! '(get (js/Map. [["my-key" 1]]) "bad-key" 3))))
    (is (identical? nil (jsv! '(get (js/Map. [[:my-key nil]]) :my-key)))))
  (testing "arrays"
    (is (eq nil (jsv! '(get ["val1" "val2" "val3"] nil))))
    (is (eq "val2" (jsv! '(get ["val1" "val2" "val3"] 1))))
    (is (identical? js/undefined (jsv! '(get ["val1" "val2" "val3"] 10))))
    (is (eq "val2" (jsv! '(get ["val1" "val2" "val3"] 10 "val2"))))
    (is (identical? nil (jsv! '(get [nil] 0)))))
  (testing "sets"
    (is (eq nil (jsv! '(get #{1 2 3} 0))))
    (is (eq nil (jsv! '(get #{1 2 3 nil} nil))))
    (is (eq 1 (jsv! '(get #{1 2 3} 1))))
    (is (eq 2 (jsv! '(get #{1 2 3} 2))))
    (is (eq 3 (jsv! '(get #{1 2 3} 3)))))
  (testing "objects"
    (is (eq nil (jsv! '(get {"my-key" 1} nil))))
    (is (eq 1 (jsv! '(get {"my-key" 1} "my-key"))))
    (is (identical? js/undefined (jsv! '(get {"my-key" 1} "bad-key"))))
    (is (eq 3 (jsv! '(get {"my-key" 1} "bad-key" 3))))
    (is (identical? nil (jsv! '(get {"my-key" nil} "my-key")))))
  (testing "arbitrary get method"
    (is (eq 1 (jsv! '(get (js/eval "class Foo { get() { return 1;} }; new Foo()") :foo))))))

(deftest first-test
  (is (= nil (jsv! '(first nil))))
  (is (= nil (jsv! '(first []))))
  (is (= nil (jsv! '(first #{}))))
  (is (= nil (jsv! '(first {}))))
  (is (= nil (jsv! '(first (js/Map. [])))))
  (is (= 1 (jsv! '(first [1 2 3]))))
  (is (= 1 (jsv! '(first #{1 2 3}))))
  (is (eq #js [1 2] (jsv! '(first (js/Map. [[1 2] [3 4]])))))
  (is (eq "a" (jsv! '(first "abc"))))
  ;; keywords are translated to strings
  (is (eq "a" (jsv! '(first :abd)))))

(deftest ffirst-test
  (is (= "f" (jsv! '(ffirst ["foo"]))))
  (is (= "f" (jsv! '(ffirst "foo")))))

(deftest rest-test
  (is (eq () (jsv! '(vec (rest nil)))))
  (is (eq () (jsv! '(vec (rest [])))))
  (is (eq () (jsv! '(vec (rest #{})))))
  (is (eq () (jsv! '(vec (rest {})))))
  (is (eq () (jsv! '(vec (rest (js/Map. []))))))
  (is (eq #js [2 3] (jsv! '(vec (rest [1 2 3])))))
  (is (eq #{2 3} (jsv! '(vec (rest #{1 2 3})))))
  (is (eq #js [#js [3 4]] (jsv! '(vec (rest (js/Map. [[1 2] [3 4]]))))))
  (is (eq '("b" "c") (jsv! '(vec (rest "abc")))))
  (is (= 1 (jsv! '(first (rest (range))))) "infinite rest"))


(deftest last-test
  (is (= nil (jsv! '(last nil))))
  (is (= nil (jsv! '(last []))))
  (is (= nil (jsv! '(last {}))))
  (is (= nil (jsv! '(last #{}))))
  (is (= nil (jsv! '(last (js/Map.)))))
  (is (= nil (jsv! '(last (map inc nil)))) "lazy iterable")
  (is (= 4 (jsv! '(last [1 2 3 4]))))
  (is (eq ["d" 4] (jsv! '(last {:a 1 :b 2 :c 3 :d 4}))))
  (is (#{1 2 3 4} (jsv! '(last #{1 2 3 4}))))
  (is (eq ["d" 4] (jsv! '(last (js/Map. [[:a 1] [:b 2] [:c 3] [:d 4]])))))
  (is (= 4 (jsv! '(last (range 5))))))

(deftest reduce-test
  (testing "no val"
    (is (= 10 (jsv! '(reduce + (range 5)))))
    (is (= 3 (jsv! '(reduce #(if (< %2 3)
                               (+ %1 %2)
                               (reduced %1))
                            (range 5))))
        "reduced early")
    (is (= 6 (jsv! '(reduce #(if (< %2 4)
                               (+ %1 %2)
                               (reduced %1))
                            (range 5))))
        "reduced last el")
    (is (= 0 (jsv! '(reduce #(reduced %1)
                            (range 5))))
        "reduced first el"))
  (testing "val"
    (is (= 15 (jsv! '(reduce #(+ %1 %2) 5 (range 5)))))
    (is (= 8 (jsv! '(reduce #(if (< %2 3)
                               (+ %1 %2)
                               (reduced %1))
                            5
                            (range 5))))
        "reduced early")
    (is (= 11 (jsv! '(reduce #(if (< %2 4)
                                (+ %1 %2)
                                (reduced %1))
                             5
                             (range 5))))
        "reduced last el")
    (is (= 5 (jsv! '(reduce #(reduced %1)
                            5
                            (range 5))))
        "reduced first el")
    (is (= 5 (jsv! '(reduce #(+ %2 %1) (reduced 5) (range 5))))
        "reduced val"))
  (testing "sets"
    (is (= 10 (jsv! '(reduce #(+ %1 %2) #{1 2 3 4})))))
  (testing "maps"
    (is (= 10 (jsv! '(reduce #(+ %1 (second %2))
                             0
                             (js/Map. [[:a 1] [:b 2] [:c 3] [:d 4]]))))))
  (testing "objects"
    (is (= 10 (jsv! '(reduce #(+ %1 (second %2))
                             0
                             (js/Object.entries {:a 1 :b 2 :c 3 :d 4})))))
    (is (= 10 (jsv! '(reduce #(+ %1 %2)
                             0
                             (js/Object.values {:a 1 :b 2 :c 3 :d 4})))))
    (is (= 10 (jsv! '(reduce #(+ %1 (second %2))
                             0
                             {:a 1 :b 2 :c 3 :d 4})))))
  (testing "empty coll"
    (is (= 0 (jsv! '(reduce + []))))))

(deftest reduced-test
  (is (jsv! '(reduced? (reduced 5))))
  (is (= 4 (jsv! '(deref (reduced 4))))))

(deftest reductions-test
  (testing "lazy"
    (is (eq (vec (take 10 (reductions + (range))))
            (jsv! `(vec (take 10 (reductions + (range))))))
        "Returns a lazy sequence"))
  (testing "no val"
    (is (eq (vec (reductions + [1 1 1 1])) (jsv! '(vec (reductions + [1 1 1 1])))))
    (is (eq (vec (reductions #(if (< %2 3)
                                (+ %1 %2)
                                (reduced %1))
                             (range 5)))
            (jsv! '(vec (reductions #(if (< %2 3)
                                       (+ %1 %2)
                                       (reduced %1))
                                    (range 5)))))
        "reduced early")
    (is (eq (reduce #(if (< %2 4)
                       (+ %1 %2)
                       (reduced %1))
                    (range 5))
            (jsv! '(reduce #(if (< %2 4)
                              (+ %1 %2)
                              (reduced %1))
                           (range 5))))
        "reduced last el")
    (is (eq (vec (reductions (fn [x _] (reduced x))
                             (range 5)))
            (jsv! '(vec (reductions (fn [x _] (reduced x))
                                    (range 5)))))
        "reduced first el"))
  (testing "val"
    (is (eq (vec (reductions conj [] '(1 2 3)))
            (jsv! '(vec (reductions conj [] '(1 2 3)))))))
  (testing "sets"
    (is (eq (vec (reductions #(+ %1 %2) #{1 2 3 4}))
            (jsv! '(vec (reductions #(+ %1 %2) #{1 2 3 4}))))))
  (testing "maps"
    (is (eq (vec (reductions #(+ %1 (second %2))
                             0
                             {:a 1, :b 2, :c 3, :d 4}))
            (jsv! '(vec (reductions #(+ %1 (second %2))
                                    0
                                    (js/Map. [[:a 1] [:b 2] [:c 3] [:d 4]])))))))
  (testing "objects"
    (is (eq (vec (reductions #(+ %1 (second %2))
                             0
                             {:a 1, :b 2, :c 3, :d 4}))
            (jsv! '(vec (reductions #(+ %1 (second %2))
                                    0
                                    (js/Object.entries {:a 1, :b 2, :c 3, :d 4}))))))
    (is (eq (vec (reductions #(+ %1 %2)
                             0
                             (vals {:a 1 :b 2 :c 3 :d 4})))
            (jsv! '(vec (reductions #(+ %1 %2)
                                    0
                                    (js/Object.values {:a 1 :b 2 :c 3 :d 4}))))))
    (is (eq (vec (reductions #(+ %1 (second %2))
                             0
                             {:a 1, :b 2, :c 3, :d 4}))
            (jsv! '(vec (reductions #(+ %1 (second %2))
                                    0
                                    {:a 1 :b 2 :c 3 :d 4}))))))
  (testing "empty coll"
    (is (eq (vec (reductions + '())) (jsv! '(vec (reductions + '())))))
    (is (eq (vec (reductions + [])) (jsv! '(vec (reductions + []))))))
  (testing "composability"
    ;; https://clojuredocs.org/clojure.core/reductions#example-58bdd686e4b01f4add58fe6b
    (is (eq (vec (take 3 (as-> (repeat {:height 50}) posts
                           (map #(assoc %1 :offset %2)
                                posts
                                (reductions + 0 (map :height posts))))))
            (jsv! '(vec (take 3 (as-> (repeat {:height 50}) posts
                                  (map #(assoc %1 :offset %2)
                                       posts
                                       (reductions + 0 (map :height posts))))))))))
  (testing "lazy-reusage"
    (is (= 100 (jsv! '(do (def a (atom []))
                          (defn spy [x] (swap! a conj x) x)
                          (vec (reductions + (map spy (range 100))))
                          (count @a)))))
    (is (eq (do
              (let [ xs (reductions + (range 3))]
                [(count xs) (count xs)]))
            (jsv! '(do
                     (let [ xs (reductions + (range 3))]
                       [(count xs) (count xs)])))))
    (is (eq 10
            (jsv! '(do (let [a (atom [])
                             spy (fn [x] (swap! a conj x) x)]
                         (doall (take 10 (reductions + (map spy (range)))))
                         (count @a)))))))
  (testing "stack consumptions"
    (is (= 49995000 (jsv! '(last (reductions + (range 10000))))))))

(deftest seq-test
  (is (= "abc" (jsv! '(seq "abc"))))
  (is (eq '(1 2 3) (jsv! '(seq [1 2 3]))))
  (is (eq '([:a 1] [:b 2]) (jsv! '(seq {:a 1 :b 2}))))
  (is (eq (js/Set. [1 2 3])
          (jsv! '(seq #{1 2 3}))))
  (is (eq (js/Map. #js[#js[1 2] #js[3 4]])
          (jsv! '(seq (js/Map. [[1 2] [3 4]])))))
  (testing "empty"
    (is (= nil (jsv! '(seq nil))))
    (is (= nil (jsv! '(seq []))))
    (is (= nil (jsv! '(seq {}))))
    (is (= nil (jsv! '(seq #{}))))
    (is (= nil (jsv! '(seq (js/Map.)))))
    (is (= nil (jsv! '(seq (map inc []))))))
  (is (eq #js [0 2 4 6 8]
          (jsv! '(loop [evens []
                        nums (range 10)]
                   (if-some [x (first nums)]
                     (recur (if (case x
                                  (0 2 4 6 8 10) true
                                  false)
                              (conj evens x)
                              evens)
                            (rest nums))
                     evens))))))

(deftest cons-test
  (is (eq [0] (jsv! '(vec (cons 0 nil)))))
  (is (eq [0 1 2 3 4] (jsv! '(vec (cons 0 [1 2 3 4])))))
  (is (eq [0 1 2 3 4] (jsv! '(vec (cons 0 (map inc (range 4)))))))
  (is (.has (jsv! '(into #{} (cons 0 #{1 2 3 4}))) 0))
  (is (eq [0 [:a 1] [:b 2]] (jsv! '(vec (cons 0 {:a 1 :b 2})))))
  (is (eq [0 [:a 1] [:b 2]] (jsv! '(vec (cons 0 (js/Map. [[:a 1] [:b 2]])))))))

(deftest map-test
  (is (eq [1 2 3 4 5] (jsv! '(vec (map inc [0 1 2 3 4])))))
  (is (every? (set (jsv! '(map inc #{0 1 2 3 4})))
              [1 2 3 4 5]))
  (is (eq [[:a 1] [:b 2]]
          (jsv! '(vec (map #(vector (first %) (inc (second %)))
                           {:a 0 :b 1})))))
  (is (eq ["A" "B" "C"]
          (jsv! '(vec (map #(.toUpperCase %) "abc")))))
  (is (eq [[0 1] [1 2] [2 3] [3 4] [4 5]]
          (jsv! '(vec (map #(vector (first %) (inc (second %)))
                           (-> [[0 0] [1 1] [2 2] [3 3] [4 4]]
                               (js/Map.)))))))
  (testing "nil"
    (is (eq () (jsv! '(vec (map inc nil)))))
    (is (eq () (jsv! '(vec (map inc js/undefined))))))
  (testing "multiple colls"
    (is (eq [4 6] (jsv! '(vec (map + [1 2] [3 4])))))
    (testing "reusing seq"
      (is (eq [2 2] (jsv! '(let [sum (map + [1 2] [3 4])]
                             [(count sum) (count sum)])))))
    (is (eq [4 6] (jsv! '(vec (map + [1 2] [3 4])))))
    (is (eq ["1y" "2o"] (jsv! '(vec (map str [1 2] "yolo")))))
    (is (eq [[1 4 7] [2 5 8] [3 6 9]]
            (jsv! '(vec (apply map vector [[1 2 3] [4 5 6] [7 8 9]])))))
    (is (eq [[1,4],[2,5],[3,6]]
            (jsv! '(vec (map vector [1 2 3] [4 5 6 7 8 9])))))
    (is (eq []
            (jsv! '(vec (map vector nil nil nil))))))
  (testing "transducer"
    (is (eq (transduce (map inc) + 0 [1 2 3])
            (jsv! '(transduce (map inc) + 0 [1 2 3]))))))

(deftest mapv-test
  (is (= true (jsv! '(vector? (mapv inc [0 1 2 3 4])))))
  (is (eq ["A" "B" "C"]
          (jsv! '(mapv #(.toUpperCase %) "abc"))))
  (testing "nil"
    (is (eq [] (jsv! '(mapv inc nil)))))
  (testing "multiple colls"
    (is (eq [4 6] (jsv! '(mapv + [1 2] [3 4]))))))

(deftest filter-test
  (is (eq [2 4 6 8] (jsv! '(vec (filter even? [1 2 3 4 5 6 7 8 9])))))
  (is (every? (set (jsv! '(vec (filter even? #{1 2 3 4 5 6 7 8 9}))))
              [2 4 6 8]))
  (is (eq [[:a 1]] (jsv! '(vec (filter #(= :a (first %)) {:a 1 :b 2})))))
  (testing "nil"
    (is (eq () (jsv! '(vec (filter even? nil)))))
    (is (eq () (jsv! '(vec (filter even? js/undefined))))))
  (testing "truthiness"
    (is (eq [{:foo 0} {:foo ""}] (jsv! '(vec (filter :foo [{:foo 0} {:foo ""} {:foo false} {:foo nil}]))))))
  (testing "transducer"
    (is (eq #js [1 3 5 7 9] (jsv! '(into [] (filter odd?) (range 10)))))))

(deftest filterv-test
  (is (= true (jsv! '(vector? (filterv even? [1 2 3 4 5 6 7 8 9])))))
  (is (eq [[:a 1]] (jsv! '(filterv #(= :a (first %)) {:a 1 :b 2})))))

(deftest remove-test
  (is (eq [2 4 6 8] (jsv! '(vec (remove odd? [1 2 3 4 5 6 7 8 9])))))
  (is (every? (set (jsv! '(vec (remove odd? #{1 2 3 4 5 6 7 8 9}))))
              [2 4 6 8]))
  (is (eq [[:a 1]] (jsv! '(vec (remove #(not= :a (first %)) {:a 1 :b 2})))))
  (testing "nil"
    (is (eq () (jsv! '(vec (remove odd? nil)))))
    (is (eq () (jsv! '(vec (remove odd? js/undefined)))))))

(deftest map-indexed-test
  (is (eq [[0 0] [1 1] [2 2] [3 3] [4 4]]
          (jsv! '(map-indexed vector [0 1 2 3 4]))))
  (is (= 20 (apply + (jsv! '(map-indexed + #{0 1 2 3 4})))))
  (is (eq [[0 :a 1] [1 :b 2]]
          (jsv! '(map-indexed #(vector %1 (first %2) (inc (second %2)))
                              {:a 0 :b 1}))))
  (is (eq [[0 "A"] [1 "B"] [2 "C"]]
          (jsv! '(map-indexed #(vector %1 (.toUpperCase %2))
                              "abc"))))
  (is (eq [[0 0 1] [1 1 2] [2 2 3] [3 3 4] [4 4 5]]
          (jsv! '(map-indexed
                  #(vector %1 (first %2) (inc (second %2)))
                  (-> [[0 0] [1 1] [2 2] [3 3] [4 4]]
                      (js/Map.))))))
  (testing "nil"
    (is (eq () (jsv! '(map-indexed vector nil))))
    (is (eq () (jsv! '(map-indexed vector js/undefined)))))
  (testing "transducer"
    (is (eq [[0 10] [1 11] [2 12] [3 13] [4 14] [5 15] [6 16] [7 17] [8 18] [9 19]]
            (jsv! "(into [] (map-indexed vector) (range 10 20))")))))

(deftest complement-test
  (is (= false (jsv! '((complement (constantly true))))))
  (is (= true (jsv! '((complement (constantly false))))))
  (is (= true (jsv! '((complement (constantly false)) "with some" "args" 1 :a))))
  (is (= true (jsv! '(let [not-contains? (complement contains?)]
                       (not-contains? [2 3 4] 5)))))
  (is (= false (jsv! '(let [not-contains? (complement contains?)]
                        (not-contains? [2 3 4] 2)))))
  (is (= true (jsv! '(let [first-elem-not-1? (complement (fn [x] (= 1 (first x))))]
                       (first-elem-not-1? [2 3])))))
  (is (= false (jsv! '(let [first-elem-not-1? (complement (fn [x] (= 1 (first x))))]
                        (first-elem-not-1? [1 2]))))))

(deftest constantly-test
  (is (= "abc" (jsv! '((constantly "abc")))))
  (is (= 10 (jsv! '((constantly 10)))))
  (is (= true (jsv! '((constantly true)))))
  (is (= nil (jsv! '((constantly nil)))))
  (is (= nil (jsv! '((constantly nil) "with some" "args" 1 :a)))))

(deftest list?-test
  (is (= true (jsv! '(list? '(1 2 3 4)))))
  (is (= true (jsv! '(list? (list 1 2 3)))))
  (is (= false (jsv! '(list? nil))))
  (is (= false (jsv! '(list? [1 2 3]))))
  (is (= false (jsv! '(list? {:a :b}))))
  (is (= false (jsv! '(list? #{:a :b})))))

(deftest vector?-test
  (is (= false (jsv! '(vector? '(1 2 3 4)))))
  (is (= false (jsv! '(vector? nil))))
  (is (= true (jsv! '(vector? [1 2 3]))))
  (is (= false (jsv! '(vector? {:a :b}))))
  (is (= false (jsv! '(vector? #{:a :b})))))

(deftest vec-test
  (is (eq [] (jsv! '(vec))))
  (is (eq [] (jsv! '(vec []))))
  (is (eq #js [0 1 2 3] (jsv! '(vec [0 1 2 3]))))
  (is (eq #js [0 1 2 3] (jsv! '(vec (range 4)))))
  (is (eq #{0 1 2 3} (jsv! '(vec #{0 1 2 3}))))
  (is (eq [["one" 1] ["two" 2]] (jsv! '(vec {:one 1 :two 2})))))

(deftest list-test
  (testing "creates a list of elements"
    (is (eq '(1 2 3) (jsv! '(list 1 2 3)))))
  (testing "accepts a single, numeric element in the list"
    (is (eq '(23) (jsv! '(list 23)))))
  (testing "creates an empty list"
    (is (eq '() (jsv! '(list))))
    (is (eq '() (jsv! "()")))
    (is (eq '() (jsv! "'()")))
    (is (eq '(1 2 3) (jsv! "'(1 2 3)")))))

(deftest instance-test
  (is (true? (jsv! '(instance? js/Array []))))
  (is (false? (jsv! '(instance? js/String []))))
  (is (true? (jsv! '(do (defn string? [x] (instance? js/String x)) (string? (new String "foo")))))))

(deftest concat-test
  (is (eq [] (jsv! '(vec (concat nil)))))
  (is (eq [1] (jsv! '(vec (concat nil [] [1])))))
  (is (eq [0 1 2 3 4 5 6 7 8 9] (jsv! '(vec (concat [0 1 2 3] [4 5 6] [7 8 9])))))
  (is (eq [["a" "b"] ["c" "d"] 2] (jsv! '(vec (concat {"a" "b" "c" "d"} [2])))))
  (testing "apply infinite seq to concat"
    (is (eq [1 2 3 1 2 3 1 2 3 1] (jsv! '(vec (take 10 (apply concat (repeat [1 2 3])))))))))

(deftest mapcat-test
  (is (eq [] (jsv! '(vec (mapcat identity nil)))))
  (is (eq [0 1 2 3 4 5 6 7 8 9] (jsv! '(vec (mapcat identity [[0 1 2 3] [4 5 6] [7 8 9]])))))
  (is (eq ["a" "b" "c" "d"] (jsv! '(vec (mapcat identity {"a" "b" "c" "d"})))))
  (testing "multiple colls"
    (is (eq ["a" 1 "b" 2] (jsv! '(vec (mapcat list [:a :b :c] [1 2])))))))

(deftest laziness-test
  (is (eq ["right" "up" "left" "left" "down" "down" "right" "right" "right" "up" "up" "up" "left" "left" "left" "left" "down" "down" "down" "down"]
          (jsv! '(do (def directions
                       "Infite seq of directions through
                        spiral: :right :up :left :left :down, etc."
                       (let [dirs (cycle [[:right :up] [:left :down]])
                             amount (map inc (range))]
                         (mapcat (fn [[d1 d2] amount]
                                   (concat (repeat amount d1)
                                           (repeat amount d2)))
                                 dirs
                                 amount)))
                     (vec (take 20 directions)))))))

(deftest interleave-test
  (is (eq [] (jsv! '(vec (interleave nil nil)))))
  (is (eq [] (jsv! '(vec (interleave [1 2] nil)))))
  (is (eq [] (jsv! '(vec (interleave [1 2 3] nil)))))
  (is (eq [] (jsv! '(vec (interleave [1 2 3] ["a" "b"] nil)))))
  (is (eq [1 "a" 2 "b"] (jsv! '(vec (interleave [1 2 3] ["a" "b"])))))
  (is (eq [1 "a" 2 "b" 3 "c"] (jsv! '(vec (interleave [1 2 3] ["a" "b" "c"])))))
  (is (eq [1 "a" 2 "b"] (jsv! '(vec (interleave [1 2] ["a" "b" "c"]))))))

(deftest interpose-test
  (is (eq [] (jsv! '(vec (interpose "," nil)))))
  (is (eq [1 ",", 2] (jsv! '(vec (interpose ",", [1 2])))))
  (is (eq [0 nil 1 nil 2] (jsv! '(vec (take 5 (interpose nil (range))))))))

(deftest select-keys-test
  (is (eq {:a 1 :b 2} (jsv! '(select-keys {:a 1 :b 2 :c 3} [:a :b]))))
  (let [m (jsv! '(select-keys (js/Map. [[:a 1] [:b 2] [:c 3]]) [:a :b]))]
    (is (instance? js/Map m))
    (is (= 1 (.get m "a")))
    (is (= 2 (.get m "b")))
    (is (not (.has m "c"))))
  (is (eq #js {} (jsv! '(select-keys nil [])))))

(deftest partition-test
  (is (eq [[0 1 2 3] [4 5 6 7] [8 9 10 11] [12 13 14 15] [16 17 18 19]] (jsv! '(vec (partition 4 (range 20))))))
  (is (eq [[0 1 2 3] [4 5 6 7] [8 9 10 11] [12 13 14 15] [16 17 18 19]] (jsv! '(vec (partition 4 (range 22))))))
  (testing "step"
    (is (eq [[0 1 2 3] [6 7 8 9] [12 13 14 15]] (jsv! '(vec (partition 4 6 (range 20)))))))
  (testing "step < n"
    (is (eq [[0 1 2 3] [3 4 5 6] [6 7 8 9] [9 10 11 12] [12 13 14 15] [15 16 17 18]] (jsv! '(vec (partition 4 3 (range 20)))))))
  (testing "pad"
    (is (eq [[0 1 2] [6 7 8] [12 13 14] [18 19 "a"]] (jsv! '(vec (partition 3 6 ["a"] (range 20))))))
    (is (eq [[0 1 2 3] [6 7 8 9] [12 13 14 15] [18 19 "a"]] (jsv! '(vec (partition 4 6 ["a"] (range 20))))))
    (is (eq [[0 1 2 3] [6 7 8 9] [12 13 14 15] [18 19 "a" "b"]] (jsv! '(vec (partition 4 6 ["a" "b" "c" "d"] (range 20)))))))
  (testing "infinite seq"
    (is (eq [[0 1 2 3] [6 7 8 9] [12 13 14 15]] (jsv! '(vec (take 3 (partition 4 6 (range)))))))
    (is (eq [[0 1 2 3] [2 3 4 5] [4 5 6 7]] (jsv! '(vec (take 3 (partition 4 2 (range)))))))))

(deftest partition-all-test
  (is (eq [[0 1 2 3] [4 5 6 7] [8 9 10 11] [12 13 14 15] [16 17 18 19]] (jsv! '(vec (partition-all 4 (range 20))))))
  (is (eq [[0 1 2 3] [4 5 6 7] [8 9 10 11] [12 13 14 15] [16 17 18 19] [20 21]] (jsv! '(vec (partition-all 4 (range 22))))))
  (testing "step"
    (is (eq [[0 1 2 3] [6 7 8 9] [12 13 14 15] [18 19]] (jsv! '(vec (partition-all 4 6 (range 20)))))))
  (testing "step < n"
    (is (eq [[0 1 2 3] [3 4 5 6] [6 7 8 9] [9 10 11 12] [12 13 14 15] [15 16 17 18] [18 19]] (jsv! '(vec (partition-all 4 3 (range 20)))))))
  (testing "infinite seq"
    (is (eq [[0 1 2 3] [6 7 8 9] [12 13 14 15]] (jsv! '(vec (take 3 (partition-all 4 6 (range)))))))
    (is (eq [[0 1 2 3] [2 3 4 5] [4 5 6 7]] (jsv! '(vec (take 3 (partition-all 4 2 (range)))))))))

(deftest partition-by-test
  (is (eq [[0] [1] [2] [3] [4] [5] [6] [7] [8] [9]]
          (js->clj (jsv! '(vec (take 10 (partition-by odd? (range))))))))
  (is (eq [ [ 1, 1, 1 ], [ 2, 2 ], [ 3, 3 ] ]
          (jsv! '(vec (partition-by odd? [1 1 1 2 2 3 3])))))
  (is (eq #js [#js [1 1 1] #js [2 2 2] #js [3 3 3]]
          (jsv! "(into [] (partition-by odd?) [1 1 1 2 2 2 3 3 3])"))))

(deftest merge-test
  (testing "corner cases"
    (is (eq [1] (jsv! '(merge [] 1))))
    (is (eq (js/Set. #js [1]) (jsv! '(merge #{} 1))))
    (is (eq '(1) (jsv! '(merge (list) 1))))
    (is (thrown? js/Error (jsv! '(merge 1 1)))))
  (is (eq {:a 1} (jsv! '(merge nil {:a 1}))))
  (is (eq {:a 1} (jsv! '(merge {:a 2} {:a 1}))))
  (is (eq {:a 1 :b 2} (jsv! '(merge {:a 1} {:b 2}))))
  (let [s (jsv! '(merge (js/Map.) {:a 1 :b 2}))]
    (is (instance? js/Map s))
    (doseq [k ["a" "b"]]
      (is (.has s k) (str "key: " k))))
  (is (true? (jsv! '(let [obj1 {:a 2}
                          obj2 (merge obj1 {:a 1})]
                      (not (identical? obj1 obj2)))))))

(deftest into-test
  (testing "corner cases"
    (is (eq [], (jsv! '(into))))
    (is (= true, (jsv! '(vector? (into nil [1])))))
    (is (eq nil, (jsv! '(into nil))))      ; same as clojure, but clojureScript throws error for arity 1
    (is (eq [1 2] (jsv! '(into nil [1 2])))))
  (testing "arrays"
    (is (eq [1 2 3 4] (jsv! '(into [1 2 3 4]))))
    (is (eq [1 2 3 4] (jsv! '(into [1 2 3] [4]))))
    (is (eq [1 2 3 4] (jsv! '(into [1 2] [3 4])))))
  (testing "lists"
    (is (eq '(1 2 3 4) (jsv! '(into '(1 2 3 4)))))
    (is (eq '(1 2 3 4) (jsv! '(into '(2 3 4) [1]))))
    (is (eq '(1 2 3 4) (jsv! '(into '(3 4) '(2 1))))))
  (testing "sets"
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(into #{1 2 3 4}))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(into #{1 2 3} [4]))))
    (is (eq (js/Set. #js [1 2 3 4]) (jsv! '(into #{1 2} [3 4])))))
  (testing "objects"
    (is (eq #js {:a "b" :c "d"} (jsv! '(into {:a "b" :c "d"}))))
    (is (eq #js {"1" 2 "3" 4} (jsv! '(into {"1" 2} [["3" 4]]))))
    (is (eq #js {"1" 2 "3" 4 "5" 6} (jsv! '(into {"1" 2} '(["3" 4] ["5" 6]))))))
  (testing "maps"
    (is (eq (js/Map. #js [#js ["a" "b"] #js ["c" "d"]])
            (jsv! '(into (js/Map. [["a" "b"] ["c" "d"]])))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4]])
            (jsv! '(into (js/Map. [[1 2]]) [[3 4]]))))
    (is (eq (js/Map. #js [#js [1 2] #js [3 4] #js [5 6]])
            (jsv! '(into (js/Map. [[1 2]]) [[3 4] [5 6]])))))
  (testing "other types"
    (is (thrown? js/Error (jsv! '(into "foo" [])))))
  (testing "xform"
    (is (eq (into [] (map inc) [1 2 3])
            (jsv! '(into [] (map inc) [1 2 3]))))
    (is (eq (new js/Set (into #{} (map inc) [1 2 3]))
            (jsv! '(into #{} (map inc) [1 2 3]))))))

(deftest iterable-protocol
  (is (eq [true true [1 2 3 4 5]]
          (jsv! '(do (deftype Foo []
                       IIterable
                       (-iterator [_]
                         ;; -iterator must return a JS Iterator object,
                         ;; not another iterable
                         (-iterator [0 1 2 3 4])))
                     (let [foo (->Foo)]
                       [(seqable? foo)
                        (= foo (seq foo))
                        (vec (map inc (->Foo)))]))))))

(deftest repeat-test
  (is (eq [] (jsv! '(vec (interleave (repeat 1) [])))))
  (is (eq [1 "a" 1 "b"] (jsv! '(vec (interleave (repeat 1) ["a" "b"])))))
  (is (eq [1 "a" 1 "b" 1 "c"] (jsv! '(vec (interleave (repeat 1) ["a" "b" "c"])))))
  (is (eq [1 "a" 1 "b"] (jsv! '(vec (interleave (repeat 2 1) ["a" "b" "c"])))))
  (testing "satisfies IIterable"
    (is (= true (jsv! '(satisfies? IIterable (repeat 1))))))
  (testing "invalid arity"
    (is (thrown? js/Error (jsv! '(repeat))))))

(deftest take-test
  (is (eq [] (jsv! '(vec (take 1 nil)))))
  (is (eq [] (jsv! '(vec (take 0 (repeat 1))))))
  (is (eq [1 1 1] (jsv! '(vec (take 3 (repeat 1))))))
  (is (eq ["a" "b"] (jsv! '(vec (take 2 ["a" "b" "c"])))))
  (is (eq ["a" "b" "c"] (jsv! '(vec (take 5 ["a" "b" "c"])))))
  (is (eq [["a" 1] ["b" 2]] (jsv! '(vec (take 2 {"a" 1 "b" 2 "c" 3})))))
  (is (= 2
         (jsv! '(let [o {:count 0}
                      s (range)
                      s' (map (fn [x]
                                (assoc! o :count (inc (:count o)))
                                x)
                              s)
                      s'' (take 2 s')]
                  (vec s'')
                  (:count o)))))
  (is (eq [0 1 2] (jsv! "(into [] (take 3) (range))"))))

(deftest take-while-test
  (is (eq [2 4] (jsv! '(vec (take-while even? [2 4 5])))))
  (let [taken (jsv! '(take-while even? [1 1 1 2 3 4]))]
    (is (eq (take-while even? [1 1 1 2 3 4]) (vec taken)))
    ;; iterating over dropped second time, still works:
    (is (eq (take-while even? [1 1 1 2 3 4]) (vec taken))))
  (is (eq (take-while odd? nil) (vec (jsv! '(take-while odd? nil))))))

(deftest take-nth-test
  (is ["a" "a" "a"] (jsv! '(vec (take 3 (take-nth 0 ["a" "b"])))))
  (is [1 4 7] (jsv! '(vec (take-nth 3 [1 2 3 4 5 6 7 8 9])))))

(deftest +-test
  (is (zero? (jsv! '(apply + []))))
  (is (= 1 (jsv! '(apply + [1]))))
  (is (= 6 (jsv! '(apply + [1 2 3]))))
  (is (= 6 (jsv! '(apply + (range 4))))))

(deftest partial-test
  (let [f (jsv! '(partial + 2))]
    (is (= 2 (f)))
    (is (= 3 (f 1)))
    (is (= 5 (f 1 1 1)))))

(deftest cycle-test
  (is (eq (take 10 (cycle [1 2 3])) (vec (jsv! '(take 10 (cycle [1 2 3])))))))

(deftest nth-test
  (is (nil? (jsv! '(nth nil 1))))
  (is (eq :default (jsv! '(nth nil 1 :default))))
  (is (= 1 (jsv! '(nth [1 2 3] 0))))
  (is (eq :default (jsv! '(nth [1 2 3] 5 :default)))))

(deftest drop-test
  (let [dropped (jsv! '(drop 3 (range 6)))]
    (is (eq [3 4 5] (vec dropped)))
    ;; iterating over dropped second time, still works:
    (is (eq [3 4 5] (vec dropped))))
  (is (eq (drop 2 nil) (vec (jsv! '(drop 2 nil))))))

(deftest drop-while-test
  (let [dropped (jsv! '(drop-while odd? [1 1 1 2 3 4]))]
    (is (eq (drop-while odd? [1 1 1 2 3 4]) (vec dropped)))
    ;; iterating over dropped second time, still works:
    (is (eq (drop-while odd? [1 1 1 2 3 4]) (vec dropped))))
  (is (eq (drop-while odd? nil) (vec (jsv! '(drop-while odd? nil))))))

(deftest distinct-test
  (doseq [coll [[1 1 1 2 3 4 1] nil]]
    (is (eq (distinct coll) (vec (jsv! `(distinct ~coll)))))))

(deftest update-test
  (is (eq {:a 2} (jsv! '(update {:a 1} :a inc))))
  (is (eq {:a 3} (jsv! '(update {:a 1} :a + 2)))))

(deftest update-in-test
  (is (eq {:a {:b 2}} (jsv! '(update-in {:a {:b 1}} [:a :b] inc))))
  (is (eq {:a {:b 4}} (jsv! '(update-in {:a {:b 2}} [:a :b] + 2))))
  (is (eq (update-in {:a {:b {}}} [:a :b :c] (fnil inc 0))
          (jsv! (update-in {:a {:b {}}} [:a :b :c] (fnil inc 0))))))

(deftest every?-test
  (is (= true (jsv! '(every? odd? nil))))
  (is (= true (jsv! '(every? odd? []))))
  (is (= true (jsv! '(every? odd? [1 3 5]))))
  (is (= false (jsv! '(every? odd? [1 3 6]))))
  (is (= true (jsv! '(every? str [1 3 6]))))
  (is (= true (jsv! '(every? str [0 1 3 6]))))
  (is (= false (jsv! '(every? identity [0 1 3 6])))))

(deftest not-every?-test
  (is (= false (jsv! '(not-every? odd? []))))
  (is (= false (jsv! '(not-every? odd? [1 3 5])))))

(deftest keep-test
  (is (eq (keep #(when (odd? %) (inc %)) [1 2 3])
          (vec (jsv! '(keep #(when (odd? %) (inc %)) [1 2 3]))))))

(deftest reverse-test
  (is (eq (reverse nil) (jsv! '(reverse nil))))
  (is (eq (reverse [1 2 3]) (jsv! '(reverse [1 2 3]))))
  (is (eq (reverse (range 10)) (jsv! '(reverse (range 10)))))
  (is (eq (let [x [1 2 3]]
            [x (reverse x)]) (jsv! '(let [x [1 2 3]]
                                      [x (reverse x)])))))

(deftest sort-test
  (is (eq (sort [3 10]) (jsv! '(sort [3 10]))))
  (is (eq (sort [4 2 3 1]) (jsv! '(sort [4 2 3 1]))))
  (is (eq (sort - [4 2 3 1]) (jsv! '(sort - [4 2 3 1]))))
  (is (eq (sort nil) (jsv! '(sort nil))))
  (is (eq (sort "zoob") (jsv! '(sort "zoob")))))

(deftest sort-by-test
  (is (eq (sort-by count ["aaa" "bb" "c"]) (jsv! '(sort-by count ["aaa" "bb" "c"]))))
  (is (eq (sort-by - [55445, 54093, 57505]) (jsv! '(sort-by - [55445, 54093, 57505])))))

(deftest shuffle-test
  (let [shuffled (jsv! '(shuffle [1 2 3 4]))]
    (doseq [i shuffled]
      (is (contains? #{1 2 3 4} i)))))

(deftest some-test
  (is (= 1 (jsv! '(some #(when (odd? %) %) [2 1 2 1])))))

(deftest not-any?-test
  (is (= true (jsv! '(not-any? odd? [2 2 6 4]))))
  (is (= true (jsv! '(not-any? odd? [])))))

(deftest replacement-test
  (is (eq (replace {:a :b} [:a :a :c :c :a :c])
          (jsv! '(replace {:a :b} [:a :a :c :c :a :c]))))
  (is (instance? js/Array (jsv! '(replace {:a :b} [:a :a :c :c :a :c]))))
  (is (eq [:b :b] (vec (jsv! '(take 2 (replace {:a :b} (repeat :a))))))))

(deftest empty?-test
  (is (jsv! '(empty? [])))
  (is (not (jsv! '(empty? [1]))))
  (is (jsv! '(empty? {})))
  (is (not (jsv! '(empty? {:a 1}))))
  (is (jsv! '(empty? #{})))
  (is (not (jsv! '(empty? #{1})))))

(deftest repeatedly-test
  (is (every? #(< % 10)
              (take 10 (jsv! '(repeatedly #(rand-int 10))))))
  (is (every? #(< % 10)
              (jsv! '(repeatedly 5 #(rand-int 10))))))

(deftest group-by-test
  (is (eq [1 3] (jsv! '(get (group-by odd? [1 2 3 4]) true))))
  (is (eq [2 4] (jsv! '(get (group-by odd? [1 2 3 4]) false))))
  (is (eq [[1 [1 2 3]]] (jsv! '(get (group-by second [[1 [1 2 3]] [2 [3 4 5]]]) [1 2 3]))))
  (is (eq #js [#js {:weight 1000, :name "John"} #js {:weight 1000, :name "Jim"}]
          (jsv! '(get (group-by
                       :weight
                       [{:weight 1000 :name :John} {:weight 1000 :name :Jim} {:weight 999 :name :Mary}]) 1000)))))

(deftest frequencies-test
  (is (eq 3 (jsv! '(get (frequencies [:a :a :b :b :b]) :b)))))

(deftest butlast-test
  (is (eq nil (jsv! '(butlast nil))))
  (is (eq nil (jsv! '(butlast []))))
  (is (eq nil (jsv! '(butlast [1]))))
  (is (eq [1] (jsv! '(butlast [1 2])))))

(deftest drop-last-test
  (is (eq [] (jsv! '(vec (drop-last nil)))))
  (is (eq [] (jsv! '(vec (drop-last [])))))
  (is (eq [] (jsv! '(vec (drop-last [1])))))
  (is (eq [1] (jsv! '(vec (drop-last 2 [1 2 3]))))))

(deftest split-at-test
  (is (eq [[1] [2 3]] (jsv! '(mapv vec (split-at 1 [1 2 3])))))
  (is (eq [[] []] (jsv! '(mapv vec (split-at 1 nil)))))
  (is (eq [[] [1 2 3]] (jsv! '(mapv vec (split-at 0 [1 2 3]))))))

(deftest map-literal-test
  (is (eq {} (jsv! '{})))
  (is (eq {"1" true} (jsv! '(do (def x 1) {x true}))))
  (is (eq {"0,1" true} (jsv! '{[0 1] true}))))

(deftest split-with-test
  (is (eq [[1] [2 3]] (jsv! '(mapv vec (split-with odd? [1 2 3])))))
  (is (eq [[] []] (jsv! '(mapv vec (split-with odd? nil)))))
  (is (eq [[0 2] [3 4]] (jsv! '(mapv vec (split-with even? [0 2 3 4]))))))

(deftest count-test
  (is (= 3 (jsv! '(count "foo"))))
  (is (= 0 (jsv! '(count nil))))
  (is (= 0 (jsv! '(count []))))
  (is (= 3 (jsv! '(count [1 2 3]))))
  (is (= 3 (jsv! '(count (take 3 (range)))))))

(deftest logic-return
  (is (= 2 (jsv! '(do (defn foo [a b] (and a b)) (foo 1 2)))))
  (is (= 1 (jsv! '(do (defn foo [a b] (or a b)) (foo 1 2))))))

(deftest logic-precedence
  (is (false? (jsv! '(and (or true false) false)))))

(deftest multiple-arity-infix
  (is (true? (jsv! '(> 5 4 3 2 1))))
  (is (true? (jsv! '(> 5 4 3))))
  (is (true? (jsv! '(> 5 4))))
  ;; I would say this is undefined in squint for now:
  #_(is (true? (jsv! '(> 5))))
  (is (eq (jsv! '(js-?? nil js/undefined 4)) 4)))

(deftest some?-test
  (is (jsv! '(some? 1)))
  (is (jsv! '(some? false)))
  (is (jsv! '(not (some? nil))))
  (is (jsv! '(not (some? js/undefined)))))

(deftest ns-test
  (is (str/includes? (squint/compile-string (pr-str '(ns foo (:require ["./popup.css" ]))))
                     "import './popup.css'"))
  (is (re-find #"import.*'./popup.css'"
               (squint/compile-string (pr-str '(ns foo (:require ["./popup.css" :as pop]))))))
  (is (str/ends-with?
       (str/trim (squint/compile-string "(ns foo (:require [\"fs\" :refer [readFileSync]])) readFileSync" {:repl true}))
       "globalThis.foo.readFileSync;"))
  (is (str/ends-with?
       (str/trim (squint/compile-string "(ns foo (:require [\"some-js-lib\" :refer [atom]])) atom" {:repl true}))
       "foo.atom;")))

(deftest default-require-test
  (let [js (squint/compile-string "(ns foo (:require [\"some-js-lib$default\" :as a :refer [atom]])) atom")]
    (is (str/includes? js "import default$1 from 'some-js-lib'"))
    (is (str/includes? js "const { atom } = default$1;")))
  (let [js (squint/compile-string "(ns foo (:require [\"some-js-lib$default\" :as a :refer [atom]])) atom" {:repl true})]
    (is (str/includes? js "var { atom } = (await import ('some-js-lib')).default;"))))

(deftest ns-test-async
  (t/async done
    (let [js (squint/compile-string "(ns foo (:require [clojure.string :as str])) (str 1 2 3 (str/join \",\" [1 2 3]))" {:repl true
                                                                                                                         :context :return})]
      (-> (.then (js/eval (str/replace "(async function () {\n%s\n})()" "%s" js))
                 (fn [v]
                   (is (= "1231,2,3" v))))
          (.finally done)))))

(deftest global-ns-test
  (t/async done
    (-> (p/let [js (squint/compile-string "(ns foo.bar) (def x 1) (ns other.ns) foo.bar/x" {:repl true
                                                                                            :context :return
                                                                                            :elide-exports true})
                v (js/eval (str/replace "(async function () {\n%s\n})()" "%s" js))]
          (is (= 1 v)))
        (p/catch (fn [err] (is (throw err))))
        (p/finally done))))

(deftest letfn-test
  (is (= 3 (jsv! '(letfn [(f [x] (g x)) (g [x] (inc x))] (f 2)))))
  (is (= 20 (jsv! '(do (defn foo [] (letfn [(g [x] (f x)) (f [x] (* 2 x))] (g 10))) (foo))))))

(deftest refer-clojure-exclude-test
  (is (= "yolo" (jsv! '(do (ns foo (:refer-clojure :exclude [assoc])) (defn assoc [m x y] :yolo) (assoc {} :foo :bar))))))

(deftest double-names-in-sig-test
  (is (= 2 (jsv! '(do (defn foo [x x] x) (foo 1 2))))))

(deftest try-catch-test
  (is (= 2 (jsv! '(try (assoc :foo 1 2) (catch :default _ 2)))))
  (is (= 2 (jsv! '(try (assoc :foo 1 2) (catch :default _ 2))))))

(deftest require-with-kebab-case-alias-test
  (let [s (squint/compile-string "(ns test-namespace (:require [\"some-js-library$default\" :as some-js-lib])) (some-js-lib/some_fn)")]
    (is (str/includes? s "import some_js_lib from 'some-js-library';"))
    (is (str/includes? s "some_js_lib.some_fn();"))
    (is (not (str/includes? s "import * as some_js_lib"))))

  (let [s (squint/compile-string "(ns test-namespace (:require [\"some-js-library\" :as some-js-lib])) (some-js-lib/some_fn)")]
    (is (str/includes? s "import * as some_js_lib from 'some-js-library'"))
    (is (str/includes? s "some_js_lib.some_fn();")))

  (let [s (squint/compile-string "(ns test-namespace (:require [\"./local_file.mjs\" :as local-file])) (local-file/some_fn)")]
    (is (str/includes? s "import * as local_file from './local_file.mjs'"))
    (is (str/includes? s "local_file.some_fn();")))
  (let [s (squint/compile-string "(ns bar (:require [\"./foo.mjs\" #_#_:refer [foo-bar] :as foo-alias])) (prn foo-alias/foo-bar)")]
    (is (str/includes? s "import * as foo_alias from './foo.mjs'"))
    (is (str/includes? s "prn(foo_alias.foo_bar);")))
  (doseq [repl [false true]]
    (let [js (squint/compile-string "(require '[clojure.string :as str-ing]) (str-ing/join [1 2 3])" {:repl repl})]
      (is (not (str/includes? js "str-ing")))
      (is (str/includes? js "str_ing"))))
  (let [s (squint/compile-string "(ns test-namespace (:require [\"some-js-library\" :refer [existsSync] :rename {existsSync exists}])) (exists \"README.md\")")]
    (is (str/includes? s "existsSync(\"README.md\")"))))

(deftest dissoc!-test
  (is (eq #js {"1" 2 "3" 4} (jsv! '(dissoc! {"1" 2 "3" 4}))))
  (is (eq #js {"1" 2 "3" 4} (jsv! '(let [x {"1" 2 "3" 4}]
                                     (dissoc! x)
                                     x))))
  (is (eq #js {"3" 4} (jsv! '(dissoc! {"1" 2 "3" 4} "1"))))
  (is (eq #js {"3" 4} (jsv! '(let [x {"1" 2 "3" 4}]
                               (dissoc! x "1")
                               x))))
  (is (eq #js {} (jsv! '(dissoc! {}))))
  (is (eq #js {} (jsv! '(let [x {"1" 2 "3" 4}]
                          (dissoc! x "1" "3")
                          x)))))

(deftest dissoc-test
  (is (eq #js {"1" 2 "3" 4} (jsv! '(dissoc {"1" 2 "3" 4}))))
  (is (eq #js {"3" 4} (jsv! '(dissoc {"1" 2 "3" 4} "1"))))
  (is (eq #js {} (jsv! '(dissoc {"1" 2 "3" 4} "1" "3")))))

(deftest js-obj-test
  (is (eq #js {} (jsv! '(js-obj))))
  (is (eq #js {:a 1} (jsv! '(js-obj :a 1))))
  (is (eq #js {:a 1 :b 2} (jsv! '(js-obj :a 1 :b 2)))))

(deftest empty-list-test
  (is (eq #js [] (jsv! "()"))))

(deftest and-or-test
  (is (eq true (jsv! '(and))))
  (is (eq nil (jsv! '(or))))
  (is (eq "0" (jsv! '(str (or 0 1)))))
  (is (eq "1" (jsv! '(str (and 0 1))))))

(deftest fn-direct-invoke-test
  (is (eq 2 (jsv! '(#(inc %) 1)))))

(defn wrap-async [s]
  (str/replace "(async function () {\n%s\n})()" "%s" s))

(deftest defclass-test
  (async done
    #_(println (jss! (str (fs/readFileSync "test-resources/defclass_test.cljs"))))
    (is (str/includes? (squint/compile-string "(defclass Foo-bar (constructor [this]))")
                       "export { Foo_bar }"))
    (is (str/includes? (:javascript (squint/compile-string* "(defclass Foo (constructor [this]))" {:repl true
                                                                                                     :context :return}))
                       "return Foo"))
    (let [source (str (fs/readFileSync "test-resources/defclass_test.cljs"))]
      (-> (p/let [v (jsv! source)
                  _ (is (= "<<<<1-3-3>>>>,1-3-3,true,false,42,4,6,1,2,foo,bar,3" (str v)))
                  state {}
                  {:keys [javascript] :as state}
                  (squint/compile-string*  "
(defclass Foo (constructor [this]) Object (toString [_] \"foo\"))"
                                           {:repl true
                                            :context :return
                                            :elide-exports true}
                                           state)
                  _ (js/eval (wrap-async javascript))
                  {:keys [_state javascript]}
                  (squint/compile-string* "(str (new Foo))"
                                          {:repl true
                                           :context :return
                                           :elide-exports true}
                                          state)
                  v (js/eval (wrap-async javascript))]
            (is (= "foo" v)))
          (p/finally done)))))

(deftest atom-test
  (is (= 1 (jsv! "(def x (atom 1)) (def y (atom 0)) (add-watch x :foo (fn [k r o n] (swap! y inc))) (reset! x 2) (remove-watch x :foo) (reset! x 3) @y"))))

(deftest override-core-var-test
  (is (= 1 (jsv! "(def count 1) (set! count (inc count)) (defn frequencies [x] (dec x)) (frequencies count)"))))

(deftest lazy-seq-test
  (is (eq #js [1] (jsv! "(vec (cons 1 (lazy-seq nil)))")))
  (is (eq #js [1 2] (jsv! "(vec (cons 1 (lazy-seq (cons 2 nil))))")))
  (testing "lazy-seq body is only evaluated once"
    (is (eq [[1 2] [1 2 3 4 5]]
            (js->clj (jsv! "(def a (atom [])) (defn log [x] (swap! a conj x) x)  (def x (lazy-seq (cons (doto 1 log) (lazy-seq (cons (doto 2 log) (vec (map inc [2 3 4]))))))) (vec x) (vec x) [@a (vec x)]"))))))

(deftest keep-indexed-test
  (is (eq #js [12 14 16 18 20] (jsv! "(keep-indexed (fn [i e] (when (odd? i) (inc e))) (range 10 20))")))
  (is (eq #js [12 14 16 18 20] (jsv! "(into [] (keep-indexed (fn [i e] (when (odd? i) (inc e)))) (range 10 20))"))))

(deftest into-array-test
  (is (eq (clj->js [[1 2 3] [1 2 3]]) (jsv! "[(into-array [1 2 3]) (into-array String [1 2 3])]"))))

(deftest iterate-test
  (is (eq (clj->js (vec (take 10 (iterate inc 0)))) (jsv! "(vec (take 10 (iterate inc 0)))"))))

(deftest juxt-test
  (is (eq #js [2 0] (jsv! "((juxt inc dec) 1)"))))

(deftest fn?-test
  (is (true? (jsv! "(fn? inc)"))))

(deftest re-seq-test
  (is (eq #js ["foo" "foo" "foo"] (jsv! "(vec (re-seq #\"foo\" \"foobfoobfoo\"))")))
  (testing "stack consumption"
    (is (eq 4000 (jsv! '(count (re-seq #"d" (apply str (repeat 4000 "d")))))))))

(deftest bit-tests
  (is (= 3 (jsv! "(+ (bit-and 1 2 3) (bit-or 1 2 3))")))
  (is (= 64 (jsv! "(bit-shift-left 8 3)"))))

(deftest alias-conflict-test
  (let [expr (fs/readFileSync "test-resources/alias_conflict_test.cljs" "UTF-8")
        js (:javascript (squint/compile-string* expr {:core-alias "squint_core"}))]
    (when (not (fs/existsSync "test-output"))
      (fs/mkdirSync "test-output"))
    (fs/writeFileSync "test-output/foo.mjs" js)
    (is (str/includes? (process/execSync "node test-output/foo.mjs")
                       "[[-1,-2,-3],true,-10]"))))

(deftest pre-post-test
  (testing "pre"
    (is (thrown-with-msg? js/Error #"Assert failed:.*number.*"
                          (jsv! "((fn [x] {:pre [(number? x)] :post [(even? %)]} (dec x)) :foo)")))
    (is (thrown-with-msg? js/Error #"Assert failed:.*number.*"
                          (jsv! "(defn foo [x] {:pre [(number? x)] :post [(even? %)]} (dec x)) (foo :foo)"))))
  (testing "post"
    (is (thrown-with-msg? js/Error #"Assert failed:.*even.*"
                          (jsv! "((fn [x] {:pre [(number? x)] :post [(even? %)]} (dec x)) 2)")))
    (is (thrown-with-msg? js/Error #"Assert failed:.*even.*"
                          (jsv! "(defn foo [x] {:pre [(number? x)] :post [(even? %)]} (dec x)) (foo 2)"))))
  (testing "all good"
    (is (= 2 (jsv! "((fn [x] {:pre [(number? x)] :post [(even? %)]} (dec x)) 3)")))
    (is (= 2 (jsv! "(defn foo [x] {:pre [(number? x)] :post [(even? %)]} (dec x)) (foo 3)")))))

(deftest def-with-docstring-test
  (is (= 2 (jsv! "(def x 1) (inc x)")))
  (is (= 2 (jsv! "(def x \"hello\" 1) (inc x)"))))

(deftest top-level-let-naming-conflict
  (is (eq #js [1 2] (jsv! "(def atm (atom [])) (let [x 1] (swap! atm conj x)) (let [x 2] (swap! atm conj x)) @atm"))))

(deftest return-zero?-branch-test
  (is (true? (jsv! "((fn [x] (zero? x)) 0)"))))

(deftest seqable?-test
  (is (true? (jsv! "(seqable? [])")))
  (is (true? (jsv! "(seqable? #{})")))
  (is (false? (jsv! "(seqable? 1)"))))

(deftest merge-with-test
  (is (eq {:a 3 :b 1 :c 1} (jsv! "(merge-with + {:a 1 :c 1} {:a 2 :b 1})"))))

(deftest fn-with-munged-args-test
  (is (eq "foo1" (jsv! "((fn [char] (str :foo char)) 1)")))
  (is (eq "foo2" (jsv! "((fn [char char] (str :foo char)) 1 2)"))))

(deftest munge-fn-name-test
  (is (fn? (jsv! "((((fn handle-enter [] handle-enter))))"))))

(deftest defonce-test
  (is (eq 1 (jsv! "(defonce x 1) x"))))

(deftest set!-test
  (is (eq 1 (jsv! "(def x {}) (set! x -foo 1) (.-foo x)"))))

(deftest min-max-key-test
  (testing "min-key"
    (is (eq {:foo 1} (jsv! "(min-key #(:foo %) {:foo 1})")))
    (is (eq {:foo 1} (jsv! "(min-key #(:foo %) {:foo 1} {:foo 2})")))
    (is (eq {:foo 1} (jsv! "(min-key #(:foo %) {:foo 2} {:foo 1})")))
    (is (eq {:foo 1} (jsv! "(min-key #(:foo %) {:foo 10} {:foo 1} {:foo 2})")))
    (is (eq {:foo 1} (jsv! "(min-key #(:foo %) {:foo 10} {:foo 2} {:foo 1})"))))
  (testing "max-key"
    (is (eq {:foo 1} (jsv! "(max-key #(:foo %) {:foo 1})")))
    (is (eq {:foo 2} (jsv! "(max-key #(:foo %) {:foo 1} {:foo 2})")))
    (is (eq {:foo 2} (jsv! "(max-key #(:foo %) {:foo 2} {:foo 1} )")))
    (is (eq {:foo 10} (jsv! "(max-key #(:foo %) {:foo 2} {:foo 10} {:foo 1})")))
    (is (eq {:foo 10} (jsv! "(max-key #(:foo %) {:foo 2} {:foo 10} {:foo 1})")))))

(deftest js-delete-test
  (is (eq {:b 2} (jsv! "(def x {:a 1 :b 2}) (js-delete x :a) x"))))

(deftest aset-test
  (is (eq [1] (jsv! "(def x []) (aset x 0 1) x")))
  (testing "multiple dimensions"
    (is (eq [[1]] (jsv! "(def x [[]]) (aset x 0 0 1) x")))
    (is (eq [[0 1]] (jsv! "(def x [[0]]) (aset x 0 1 1) x")))))

(deftest toFn-test
  (testing "keywords"
    (is (eq [true false] (vec (jsv! '(map :foo [{:foo true :a 1} {:foo false :a 2}])))))
    (is (eq [#js {:foo true, :a 1}] (vec (jsv! '(filter :foo [{:foo true :a 1} {:foo false :a 2}])))))
    (is (eq [1 2] (vec (jsv! '((juxt :foo :bar) {:foo 1 :bar 2}))))))
  (testing "colls"
    (is (eq ["foo" "bar" nil] (vec (jsv! '(map #{:foo :bar} [:foo :bar :baz])))))
    (is (eq ["foo" "bar"] (vec (jsv! '(keep #{:foo :bar} [:foo :bar :baz])))))))

(deftest core-var-conflict-with-local-test
  (is (true? (jsv! '(let [truth_ 1] (clojure.core/truth_ 1))))))

(deftest condp-test
  (is (eq 3 (jsv! '(condp some [1 2 3 4]
                     #{0 6 7} :>> inc
                     #{4 5 9} :>> dec
                     #{1 2 3} :>> #(+ % 3))))))

(deftest nil?-test
  (let [js (jss! "(nil? 1)")]
    (is (str/includes? js "== null"))
    (is (false? (js/eval js)))))

(deftest re-find-test
  (is (eq nil (jsv! '(re-find #"foo." "dude"))))
  (is (eq "foox" (jsv! '(re-find #"foo." "xfooxbar"))))
  (is (eq (re-find #"(\D+)|(\d+)" "word then number 57")
          (vec (jsv! '(re-find #"(\D+)|(\d+)" "word then number 57"))))))

(deftest re-pattern-test
  (is (eq #"\d+" (jsv! '(re-pattern "\\d+"))))
  (is (eq "dgimsuy" (jsv! '(.-flags (re-pattern "(?dgimsuy)foo")))))
  (is (eq "dgi" (jsv! '(.-flags (re-pattern "(?dgi)foo")))))
  (is (eq "foo" (jsv! '(.-source (re-pattern "(?dgi)foo"))))))

(deftest js-in-test
  (is (true? (jsv! "(js-in :foo {:foo 1})"))))

(deftest int-test
  (is (= 3 (jsv! "(int 3.14)"))))

(deftest math-hof-test
  (is (= 6 (jsv! "(apply * [1 2 3])")))
  (is (true? (jsv! "(apply < [1 2 3])")))
  (is (true? (jsv! "(apply <= [1 1 2 3])")))
  (is (true? (jsv! "(apply > (reverse [1 2 3]))")))
  (is (true? (jsv! "(apply >= (reverse [1 1 2 3]))")))
  (is (true? (jsv! "(apply = [1 1 1])")))
  (is (false? (jsv! "(apply = [1 1 2])"))))

(deftest zipmap-test
  (is (eq #js {} (jsv! "(zipmap nil [1 2 3])")))
  (is (eq #js {} (jsv! "(zipmap [1 2 3] nil)")))
  (is (eq #js {"0" 1, "1" 2, "2" 3} (jsv! "(zipmap (range) [1 2 3])")))
  (is (eq #js {"1" 0, "2" 1, "3" 2} (jsv! "(zipmap [1 2 3] (range))"))))

(deftest not-empty-test
  (is (nil? (jsv! "(not-empty \"\")")))
  (is (nil? (jsv! "(not-empty {})")))
  (is (= "foo" (jsv! "(not-empty \"foo\")")))
  (is (eq {:a 1} (jsv! "(not-empty {:a 1})"))))

(deftest not-test
  (is (false? (jsv! "(not 0)")))
  (is (false? (jsv! "(not \"\")")))
  (is (not (str/includes? (jss! "(not (zero? 1))") "not"))))

(deftest reduce-kv-test
  (is (eq {1 :a, 2 :b, 3 :c} (jsv! "(reduce-kv #(assoc %1 %3 %2) {} {:a 1 :b 2 :c 3})")))
  (is (eq {1 :a, 2 :b, 3 :c} (jsv! "(reduce-kv #(assoc %1 %3 %2) {} (new js/Map (js/Object.entries {:a 1 :b 2 :c 3})))")))
  (is (eq {:a 1} (jsv! "(reduce-kv #(assoc %1 %3 %2) {:a 1} {})"))))

(deftest set-lib--test
  (t/async done
    (->
     (p/do
       (testing "intersection"
         (let [set (fn [& xs] (new js/Set xs))
               js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
             [(set/intersection #{:a :b})
              (set/intersection #{:a :b} #{:b :c})]" {:repl true
                                                      :context :return})]
           (p/let [vs (js/eval (wrap-async js))]
             (let [expected [(set "a" "b") (set "b")]
                   pairs (map vector expected vs)]
               (doseq [[expected s] pairs]
                 (is (eq expected s)))))))
       (testing "difference"
         (let [set (fn [& xs] (new js/Set xs))
               js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                 [(set/difference)
                  (set/difference #{:a :b})
                  (set/difference #{:a :b} #{:b :c})]" {:repl true
                                                        :context :return})]
           (p/let [vs (js/eval (wrap-async js))]
             (let [expected [nil
                             (set "a" "b")
                             (set "a")]
                   pairs (map vector expected vs)]
               (doseq [[expected s] pairs]
                 (is (eq expected s)))))))
       (testing "union"
         (let [set (fn [& xs] (new js/Set xs))
               js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                   [(set/union)
                    (set/union #{:a :b})
                    (set/union #{:a :b} #{:b :c})]" {:repl true
                                                     :context :return})]
           (p/let [vs (js/eval (wrap-async js))]
             (let [expected [nil
                             (set "a" "b")
                             (set "a" "b" "c")]
                   pairs (map vector expected vs)]
               (doseq [[expected s] pairs]
                 (is (eq expected s)))))))
       (testing "subset"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                   [(set/subset?)
                    (set/subset? #{:a :b})
                    (set/subset? #{:a :b} #{:a :b :c})
                    (set/subset? #{:a :b :d} #{:a :b :c})
                    (set/subset? #{:a :b} #{:a :b})]" {:repl true
                                                       :context :return})
                 vs (js/eval (wrap-async js))]
           (let [expected [true
                           false
                           true
                           false
                           true]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s))))))
       (testing "superset?"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                     [(set/superset?)
                      (set/superset? #{:a :b})
                      (set/superset? #{:a :b :c} #{:a :b})
                      (set/superset? #{:a :b :c} #{:a :b :d})
                      (set/superset? #{:a :b} #{:a :b})]" {:repl true
                                                           :context :return})
                 vs (js/eval (wrap-async js))]
           (let [expected [true
                           true
                           true
                           false
                           true]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s))))))
       (testing "select"
         (p/let [set (fn [& xs] (new js/Set xs))
                 js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                   [(set/select)
                    (set/select even?)
                    (set/select identity #{0 1 2 3})
                    (set/select even? #{1 2 3 4 5})
                    (set/select #(> % 2) #{1 2 3 4 5})
                    (set/select #(= % :a) #{:a :b :c})]" {:repl true
                                                          :context :return})
                 vs (js/eval (wrap-async js))]
           (let [expected [nil
                           nil
                           (set 0 1 2 3)
                           (set 2 4)
                           (set 3 4 5)
                           (set "a")]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s)))))))
       (testing "rename-keys"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                 [(set/rename-keys {:a 1, :b 2} {:a :new-a, :b :new-b})
                  (set/rename-keys {:a 1} {:b :new-b})
                  (set/rename-keys {:a 1 :b 2}  {:a :b :b :a})
                  (set/rename-keys (new js/Map [[:a {:b 1}]]) {:a {:c 2}})]" {:repl true
                                                                              :context :return})
                 vs (js/eval (wrap-async js))]
           (let [expected [{:new-a 1, :new-b 2}
                           {:a 1}
                           {:b 1, :a 2}
                           (new js/Map (clj->js [[{:c 2} {:b 1}]]))]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s)))))))
       (testing "rename"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                        [(set/rename #{ {:a 1, :b 2} {:a 3, :b 4} } {:a :new-a, :b :new-b})
                         (set/rename #{ {:a 1} {:a 2 :b 3} } {:b :new-b})
                         (set/rename #{ {:a 1 :b 2} {:a 3 :b 4} }  {:a :b :b :a})
                         (set/rename #{ (new js/Map [[:a {:b 1}]]) } {:a {:c 2}})]" {:repl true
                                                                                     :context :return})
                 vs (js/eval (wrap-async js))]
           (let [set (fn [& xs] (new js/Set xs))
                 expected [(set #js {:new-a 1, :new-b 2} #js {:new-a 3, :new-b 4})
                           (set #js {:a 1} #js {:a 2 :new-b 3})
                           (set #js {:b 1, :a 2} #js {:b 3, :a 4})
                           (set (new js/Map (clj->js [[{:c 2} {:b 1}]])))]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s)))))))
       (testing "project"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                        [(set/project #{ {:a 1, :b 2, :c 3} {:a 4, :b 5, :c 6} } [:a :b])
                         (set/project #{ {:a 1 :b 2} {:a 4 :b 5 :c 6} }  [:a :b :c :d])
                         (set/project #{ (new js/Map [[:a 1] [:d 3]]) } [:a])]" {:repl true
                                                                                 :context :return})
                 vs (js/eval (wrap-async js))]
           (let [set (fn [& xs] (new js/Set xs))
                 expected [(set #js {:a 1, :b 2} #js {:a 4, :b 5})
                           (set #js {:a 1, :b 2} #js {:a 4, :b 5, :c 6})
                           (set (new js/Map (clj->js [[:a 1]])))]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s)))))))
       (testing "map-invert"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                               [(set/map-invert)
                                (set/map-invert {})
                                (set/map-invert {:a 1, :b 2, :c 3})
                                (set/map-invert (new js/Map [[:a 1] [:d 3]]))]" {:repl true
                                                                                 :context :return})
                 vs (js/eval (wrap-async js))]
           (let [expected [{}
                           {}
                           {1 :a 2 :b 3 :c}
                           (new js/Map (clj->js [[1 :a] [3 :d]]))]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s)))))))
       (testing "join"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                      [(set/join #{ {:a 1} {:a 2} } #{ {:b 1} {:b 2} })
                       (set/join #{ {:name \"betsy\" :owner \"brian\" :kind \"cow\"}
                                     {:name \"jake\"  :owner \"brian\" :kind \"horse\"}
                                     {:name \"josie\" :owner \"dawn\"  :kind \"cow\"} }
                                 #{ {:kind \"cow\" :personality \"stoic\"}
                                    {:kind \"horse\" :personality \"skittish\"} })
                       (set/join #{ {:name \"betsy\" :owner \"brian\" :kind \"cow\"}
                                     {:name \"jake\"  :owner \"brian\" :kind \"horse\"}
                                     {:name \"josie\" :owner \"dawn\"  :kind \"cow\"} }
                                 #{ {:species \"cow\" :personality \"stoic\"}
                                    {:species \"horse\" :personality \"skittish\"} }
                                 {:kind :species})]" {:repl true
                                                      :context :return})
                 vs (js/eval (wrap-async js))]
           (let [set (fn [& xs] (new js/Set xs))
                 expected [(set #js {:a 1, :b 1} #js {:a 1, :b 2} #js {:a 2, :b 1} #js {:a 2, :b 2})
                           (set #js {:name "betsy", :owner "brian", :kind "cow", :personality "stoic"}
                                #js {:name "jake", :owner "brian", :kind "horse", :personality "skittish"}
                                #js {:name "josie", :owner "dawn", :kind "cow", :personality "stoic"})
                           (set #js {:name "betsy", :owner "brian", :kind "cow", :species "cow", :personality "stoic"}
                                #js {:name "jake", :owner "brian", :kind "horse", :species "horse", :personality "skittish"}
                                #js {:name "josie", :owner "dawn", :kind "cow", :species "cow", :personality "stoic"})]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s)))))))
       (testing "join-renaming-keys"
         (p/let [js (squint/compile-string "(ns foo (:require [clojure.set :as set]))
                      [(set/project
                       (set/join #{{:user-id 2, :name \"jake\", :age 28, :type \"company\"}
                                   {:user-id 3, :name \"amanda\", :age 63, :type \"personal\"}
                                   {:user-id 1, :name \"john\", :age 22, :type \"personal\"}}
                                 (set/rename #{{:acc-id 2, :user-id 2, :amount 1200, :type \"saving\"}
                                               {:acc-id 3, :user-id 1, :amount 850.1, :type \"debit\"}
                                               {:acc-id 1, :user-id 1, :amount 300.45, :type \"saving\"}}
                                             {:type :atype}))
                       [:user-id :acc-id :type :atype])]" {:repl true
                                                           :context :return})
                 vs (js/eval (wrap-async js))]
           (let [set (fn [& xs] (new js/Set xs))
                 expected [(set #js {:user-id 1, :acc-id 1, :type "personal", :atype "saving"}
                                #js {:user-id 2, :acc-id 2, :type "company", :atype "saving"}
                                #js {:user-id 1, :acc-id 3, :type "personal", :atype "debit"})]
                 pairs (map vector expected vs)]
             (doseq [[expected s] pairs]
               (is (eq expected s) (str "expected vs actual:"
                                        (util/inspect expected) (util/inspect s))))))))
     (p/finally done))))

(deftest Symbol_iterator-is-destructurable-test
  (let [js-obj (js/eval "
function define(obj, props) {
    for (const key of Reflect.ownKeys(props)) {
        const { get, set, value } = Object.getOwnPropertyDescriptor(props, key);
        let desc =
            get || set
                ? { get, set, configurable: true }
                : { value, writable: true, configurable: true };
        Object.defineProperty(obj, key, desc);
    }
}

class Foo {
  [Symbol.iterator]() {
    return { next: () => ({value: 1, done: false}) };
  }
};


new Foo();")
        f (jsv! '(let [f (fn [js-obj]
                           (let [[x y] js-obj]
                             [x x y y]))]
                   f))]
    (is (eq [1 1 1 1] (f js-obj)))))

(deftest flatten-test
  (is (eq [1 2 3 4 3] (jsv! '(vec (flatten '(1 2 (3 (4 (((3)))))))))))
  (is (eq 0 (jsv! '(first (flatten (map range (range))))))))

(deftest counted?-test
  (is (true? (jsv! '(counted? {})))))

(deftest persistent!-test
  (is (eq {} (jsv! '(persistent! (transient {})))))
  (is (thrown? js/Error (jsv! '(assoc! (persistent! (transient {})) :a 1))))
  (is (eq {:a 1} (jsv! '(assoc! (transient (persistent! (transient {}))) :a 1)))))

(deftest sorted-set-test
  (is (eq -10 (first (jsv! '(sorted-set 1 2 3 -10)))))
  (is (eq -10 (first (jsv! '(sorted-set 1 2 3 -10)))))
  (is (eq [-10000 -1000 1 100] (jsv! '(vec (conj (disj (sorted-set 1 -10 100 -1000) -10) -10000 -10000))))))

(deftest subseq-test
  (is (eq [1] (jsv! '(subseq (sorted-set 1 2 3 4) < 2))))
  (is (eq [3 4] (jsv! '(subseq (sorted-set 1 2 3 4) > 2))))
  (is (eq [104] (jsv! '(subseq (sorted-set 3 27 49 70 71 104) > 87 < 128))))
  (is (eq [4 5 6 7 8 9] (jsv! '(subseq (sorted-set 1 2 3 4 5 6 7 8 9 0) <= 4 >= 2)))))

(deftest cat-test
  (is (eq [1 2 3 4 5 6] (jsv! '(into [] cat [[1 2 3] [4 5 6]])))))

(deftest pragmas-test
  (let [code "\"use client\"
(js* \"// ts-check\")
(js* \"'use server'\")
(js* \"/**
* @param {number} x
*/\")
(defn foo [x] (merge x nil))
\"use serverless\"
"]
    (doseq [code [code (str/replace "(do %s)" "%s" code)]]
      (let [{:keys [pragmas javascript]} (squint/compile-string* code)]
        (is (str/includes? pragmas "use client"))
        (is (str/includes? pragmas "// ts-check"))
        (is (not (str/includes? pragmas ";")))
        (is (< (str/index-of javascript "use client")
               (str/index-of javascript "ts-check")
               (str/index-of javascript "'use server'")
               (str/index-of javascript "import")
               (str/index-of javascript "@param")
               (str/index-of javascript "use serverless")))))))

(deftest js-doc-compat-test
  (let [js (squint/compile-string "(js* \"/**\n* @param {number} x\n*/\") (defn foo [x] x)")]
    (is (str/includes? js "var foo = function"))))

(deftest memoize-test
  (is (eq #js [1 #js [1 2] 3]
          (jsv! "(def state (atom 0))
                 (defn foo ([x] (swap! state inc) x) ([x y] (swap! state inc) [x y]))
                 (def foo (memoize foo))
                 (foo 1) (foo 1 2) (foo 2 3) [(foo 1) (foo 1 2) @state]"))))

(deftest peek-pop-test
  (is (= 3 (jsv! '(peek [1 2 3]))))
  (is (eq [1 2] (jsv! '(pop [1 2 3])))))

(deftest gen-test
  (is (eq [0 1 2 3 4 5 6]
          (jsv! "(defn ^:gen foo [] (let [f (fn [] 0)] (js-yield (f))) (js-yield 1) (js-yield 2) (js-yield (let [x (do (js-yield 3) 4)] x)) (js-yield* [5 6])) (vec (foo))")))
  (testing "varargs"
    (is (eq [0 1 2 3] (jsv! "(defn ^:gen foo [& xs] (js-yield 0) (js-yield* xs)) (vec (foo 1 2 3))"))))
  (testing "multi-arity"
    (is (eq [6 7] (jsv! "(defn ^:gen foo ([] (js-yield (+ 1 2 3))) ([x] (js-yield x))) (into [] cat [(foo) (foo 7)])"))))
  (testing "multi-arirt + variadic"
    (is (eq [6 7 8 9] (jsv! "(defn ^:gen foo ([] (js-yield (+ 1 2 3))) ([x & xs] (js-yield x) (js-yield* xs))) (into [] cat [(foo) (foo 7 8 9)])"))))
  (is (eq [1 2] (jsv! "(vec ((^:gen fn [] (js-yield 1) (js-yield 2))))")))
  (is (eq [1 2] (jsv! "(vec ((fn ^:gen foo [] (js-yield 1) (js-yield 2))))"))))

(deftest infix-return-test
  (is (true? (jsv! "(defn foo [x] (and (int? x) (< 10 x 18))) (foo 12)"))))

(deftest no-private-export-test
  (let [exports (:exports (squint/compile-string* "(defn- foo []) (defn bar [])"))]
    (is (not (str/includes? exports "foo")))
    (is (str/includes? exports "bar"))))

(deftest use-existing-alias-test
  (testing "single-word alias"
    (let [s (squint/compile-string "(require '[my.foo-bar :as foo]) (foo/some-fn)")]
      (is (str/includes? s "import * as foo from 'my.foo-bar'"))
      (is (str/includes? s "foo.some_fn();"))))
  (testing "munged alias"
    (let [s (squint/compile-string "(require '[my.foo-bar :as foo-bar]) (foo-bar/some-fn)")]
      (is (str/includes? s "import * as foo_bar from 'my.foo-bar'"))
      (is (str/includes? s "foo_bar.some_fn();")))))

(deftest infix-issue-483-test
  (is (= 1 (jsv! "(/ 10 (when (odd? 3) 10))"))))

(deftest fn-params-destructuring-test
  (let [s (squint/compile-string "(defn foo [^:js {:keys [x y]}] [x y])")]
    (is (str/includes? s "{x,y}"))))

(deftest arrow-fn-test
  (is (true? (jsv! "(def obj {:a (fn [] (this-as this this))}) (= obj (.a obj))")))
  (is (true? (jsv! "(def obj {:a ^:=> (fn [] (this-as this this))}) (not= obj (.a obj))")))
  (is (true? (jsv! "(def obj {:a (^:=> fn [] (this-as this this))}) (not= obj (.a obj))")))
  (is (true? (jsv! "(def obj {:a (fn ^:=> [] (this-as this this))}) (not= obj (.a obj))")))
  (testing "no paren wrapping"
    (is (str/starts-with? (:body (squint/compile-string* "(fn ^:=> [] 1)")) "() =>"))))

(deftest alias-test
  (is (str/includes?
       (squint/compile-string "(m/dude)" {:aliases {'m 'malli.core}})
       "malli.core.dude()")))

(deftest interop-test
  (is (= 1 (jsv! "(defn foo [x] x.a) (foo {:a 1})"))))

(deftest issue-537-test
  (is (true? (jsv! "(not (= 1 2))"))))

(deftest delay-test
  (is (eq [0 2 1 2 1]
          (jsv! "(def a (atom 0)) (def x (delay (do (swap! a inc) 2))) [@a @x @a @x @a]"))))

(deftest map?-test
  (is (eq [true true false]
          (jsv! "[(map? {}) (map? (new Map [])) (map? [])]"))))

(deftest clj->js-test
  (is (eq [2 3 4]
          (jsv! "(clj->js (map inc [1 2 3]))")))
  (is (eq {:a [2 3 4]}
          (jsv! "(clj->js {:a (map inc [1 2 3])})")))
  (is (eq [2 3 4]
          (jsv! "(def x {:a 1 :b (map inc [1 2 3])}) (set! (.-a x) x) (:b (clj->js x))")))
  (is (true?
       (jsv! "(defclass Foo (constructor [this x] (set! this.x x))) (instance? Foo (clj->js (new Foo [1 2 3])))")))
  (is (true?
       (jsv! "(= \"foo\" (clj->js \"foo\"))"))))

(deftest divide-single-arg-test
  (is (= 0.5 (jsv! "(/ 2)")))
  (is (= 0.5 (jsv! "(let [f (fn [] (/ 2))] (f))"))))

(deftest issue-599-test
  (is (eq [1 2 3] (jsv! "(def f #(apply vector %&)) (f 1 2 3)"))))

(defn init []
  (t/run-tests 'squint.compiler-test 'squint.jsx-test 'squint.string-test 'squint.html-test 'squint.nrepl-test))
