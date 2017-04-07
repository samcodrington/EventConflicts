(ns kiraprob.core)

(defn gen-vector! [num max]
  (loop [i 0 events (vector)]
    (if (>= i num)
      events
     (recur (inc i)             
              (conj events 
                    (let [start (rand-int max)];; create start time between 0 and max
                      (let [interim (inc (rand-int (- max start)))]
                        [start (+ interim start)]  ;;2nd arg is end time between start and max)
                        )
                      )
               )
     )
     
    )
  )
)
(defn sort-vector [vector]
  (let [out-vec (sort-by first vector)] out-vec) ;;now list is ordered first to last in order of start time
)


(defn print-conflicts [events num]
  (println "Printing Conflicts")
  (def i (atom 0))
  (while (< @i num)
	  (let [cur_event (nth events @i)]
	     (def j  (atom (inc @i)))
       (while (< @j num)
         (let [test_event (nth events @j)]
           (if  ;if an event starting after the current event begins before the current event ends or starts at the same time
	             (or (> (second cur_event) (first test_event))(= (first cur_event) (first test_event)))
			       
             (println (str "Conflict between: Event #" (inc @i) " and #" (inc @j) " ("  cur_event " and " test_event ")"))
             (println "No Conflict between: Event #" (inc @i) " and #" (inc @j))
			     )
         )
         (swap! j inc)
       )
	     
    )
    (swap! i inc)
    )
   
)
  

(defn -main [num max]
  (println "Start!")
  (def start (gen-vector! num max))
  (println start)
  (println "Sort!")
  (def sorted (sort-vector start))
  (println sorted)
  (print-conflicts sorted num)  
)
