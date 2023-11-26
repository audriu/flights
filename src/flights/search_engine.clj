(ns flights.search-engine
  (:require [clojure.string :as str]))
; Define a map of available flights where each key is a vector representing a route
; from a departure city to a destination city, and each value is a map with the price
; and number of connections for that flight.
; (ica.ica1/main-function "Prague,Zadar,family")

(def flights
  {["Prague" "Vienna"] {:price 100 :connections 1}
   ["Vienna" "Zadar"]  {:price 200 :connections 1}
   })

; Define the basic requirements for families and groups. Here, we use a map where the
; keys are the type of passenger group ('family' or 'group') and the values are maps
; that specify the maximum number of connections and the budget.
(def requirements
  ; TODO EL: probably "f" and "g" respectively
  {:family {:max-connections 2 :max-budget 700}
   :group  {:max-connections 3 :max-budget 1000}})

; Example function to check if a route meets the family or group requirements
(defn route-meets-requirements?
  [route-type route]
  (let [reqs (requirements route-type)]
    (and (<= (:connections route) (:max-connections reqs))
         (<= (:price route) (:max-budget reqs)))))


(defn find-flights
  "Returns a list of possible routes from `start` to `end` within the given
  `max-connections` and `max-price`."
  [start end max-connections max-price]
  ; A helper function to extend the current path with a new flight.
  (letfn [(extend-path [current-path flight]
            (let [current-city (first flight)
                  next-city (second flight)
                  flight-info (flights flight)]
              (conj current-path
                    {:from        current-city
                     :to          next-city
                     :price       (:price flight-info)
                     :connections (:connections flight-info)})))]
    ; The core recursive DFS function.
    (loop [paths-to-visit (list [{:path [] :total-price 0 :total-connections 0}])
           valid-paths []]
      (if (empty? paths-to-visit)
        ; If there are no more paths to visit, return the valid paths found
        valid-paths
        ; Otherwise, continue with DFS
        (let [{:keys [path total-price total-connections]} (first paths-to-visit)
              remaining-paths (rest paths-to-visit)
              current-city (if (empty? path) start (:to (last path)))]
          (if (= current-city end)
            ; If the current path has reached the end, add it to the valid paths
            (recur remaining-paths (conj valid-paths path))
            ; If not at the end, find all possible next flights and extend the path
            (let [; Find flights departing from the current city.
                  next-flights (filter #(= (first %) current-city) (keys flights))

                  ; Filter out flights leading to already visited cities in the path
                  new-destinations (filter #(not (contains? (set (map :to path)) (second %))) next-flights)

                  ; Further filter out any flights that exceed budget or connection constraints
                  feasible-flights (filter #(let [info (flights %)]
                                              (and (<= (+ total-price (:price info)) max-price)
                                                   (<= (+ total-connections (:connections info)) max-connections)))
                                           new-destinations)]

              (recur (concat (map #(assoc % :path (extend-path path %)
                                            :total-price (+ total-price (:price (flights %)))
                                            :total-connections (+ total-connections (:connections (flights %))))
                                  feasible-flights)
                             remaining-paths)
                     valid-paths))))))))

(defn format-travel-plans
  "Formats a list of travel plans for display."
  [travel-plans]
  ; Define a local function to format a single travel plan
  (let [format-plan (fn [plan]
                      (str "Travel Plan:\n"
                           "  From: " (:departure plan) "\n" ; Append the departure city
                           "  To: " (:destination plan) "\n" ; Append the destination city
                           "  Total Price: $" (:total-price plan) "\n" ; Append the total price
                           "  Total Connections: " (:total-connections plan) "\n"))] ; Append the total number of connections
    ; Apply the format-plan function to each travel plan in the list and concatenate the results into a single string
    (apply str (map format-plan travel-plans))))



; Assume the following functions are defined:
; - parse-and-validate-input: Parses and validates user input
; - find-flights: Finds feasible flights based on criteria
; - format-travel-plans: Formats the list of travel plans for display

(defn validate-input
  "Parses and validates user input. Expected input format: 'departure-city,destination-city,passenger-type'."
  [input-string]
  (let [parsed-input (clojure.string/split input-string #",")
        [departure destination type] parsed-input
        is-valid (atom true)
        ]
    (println "departure =" departure "; destination =" destination "; type =" type)
    (when (or (nil? departure) (empty? departure))
      (println "Departure city is missing")
      (reset! is-valid false)
      )
    (when (or (nil? destination) (empty? destination))
      (println "Destination city is missing")
      (reset! is-valid false)
      )
    ; TODO EL: here can be potential problem with contains? -- > it works in different way, not like in JS/Python
    (when (not (contains? #{:family :group} (keyword type)))
      (println "Group is missing")
      (reset! is-valid false)
      )
    @is-valid
    )
  )

(defn main-function
  "The main function that ties everything together in the travel application."
  [input-string]
  (try
    ; Parse and validate the input
    (let [parsed-input (clojure.string/split input-string #",")
          [departure destination type] parsed-input
          ; Retrieve the requirements based on the type of passengers (family or group)
          reqs (requirements type)
          max-connections (:max-connections reqs)
          max-price (:max-budget reqs)
          ; Find feasible travel plans based on the criteria
          travel-plans (find-flights departure destination max-connections max-price)]

      ; Format and return the travel plans for display
      (format-travel-plans travel-plans))

    ; Catch and handle IllegalArgumentExceptions
    (catch IllegalArgumentException e
      (str "Error: " (.getMessage e)))))

; EL: here we launch the program (f-family)
(def input-string "Prague,Zadar,f")
(when (validate-input input-string)
  (main-function input-string)
  )

; First, require our namespace
; (require 'ica.ica1)

; (require '[ica.ica1 :refer :all])


; Now, you can call the main function with different inputs to test it
; (ica.ica1/main-function "Prague,Zadar,family")