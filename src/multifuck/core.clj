(ns multifuck.core
  (:gen-class))

(def OP-POINTER-INC \>)
(def OP-POINTER-DEC \<)
(def OP-VALUE-INC \+)
(def OP-VALUE-DEC \-)
(def OP-OUTPUT \.)
(def OP-INPUT \,)
(def OP-WHILE-OPEN \[)
(def OP-WHILE-CLOSE \])

(defn initialize [memory-size]
  {:program nil :memory (vec (take memory-size (repeat 0))) :data-ptr 0
   :instruction-ptr 0 :operations 0 :while-stack (list)})

(defn load-program [state program] (assoc state :program program))
(defn instruction-pointer-inc [state] (update-in state [:instruction-ptr] inc))
(defn instruction-pointer-reset [state] (assoc state :instruction-ptr (first (:while-stack state))))
(defn data-pointer-inc [state] (update-in state [:data-ptr] inc))
(defn data-pointer-dec [state] (update-in state [:data-ptr] dec))
(defn value-inc [state] (update-in state [:memory (:data-ptr state)] inc))
(defn value-dec [state] (update-in state [:memory (:data-ptr state)] dec))
(defn value [state] (get-in state [:memory (:data-ptr state)]))
(defn instruction [state] (get-in state [:program (:instruction-ptr state)]))
(defn output [state] (print (char (get-in state [:memory (:data-ptr state)]))) state)
(defn input [state]
  ;;; Figure out a good way to do this in Java.
  state)

(defn move-while-close [state]
  (if (= (instruction state) OP-WHILE-CLOSE) state (recur (instruction-pointer-inc state))))

(defn while-open [state]
  (if (= (value state) 0)
    (move-while-close state) (update-in state [:while-stack] conj (:instruction-ptr state))))

(defn while-close [state]
  (if (not= (value state) 0) (instruction-pointer-reset state) (update-in state [:while-stack] rest)))

(def operation-map
  {OP-POINTER-INC data-pointer-inc
   OP-POINTER-DEC data-pointer-dec
   OP-VALUE-INC value-inc
   OP-VALUE-DEC value-dec
   OP-OUTPUT output
   OP-INPUT input
   OP-WHILE-OPEN while-open
   OP-WHILE-CLOSE while-close})

(defn execute-command [state]
  (let [current-instruction (instruction state)
        run-fn (operation-map current-instruction)]
    (when (not (fn? run-fn))
      (ex-info "Command function not found." state))
    (run-fn state)))

(defn step [state]
  (-> state
      (execute-command)
      (instruction-pointer-inc)
      (update-in [:operations] inc)))

(defn run [state]
  (let [current-instruction (instruction state)]
    (if (nil? current-instruction)
      {:status "complete"
       :operations (:operations state)}
      (recur (step state)))))

(def HELLO-WORLD "++++++++[>++++[>++>+++>+++>+<<<<-]>+>+>->>+[<]<-]>>.>---.+++++++..+++.>>.<-.<.+++.------.--------.>>+.>++.")

(defn -main
  [& args]
  (println "Multifuck 0.1.0")
  (-> (initialize 32)
        (load-program HELLO-WORLD)
        (run)))

