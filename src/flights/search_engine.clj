(ns flights.search-engine
  (:require [clojure.string :as str]))

; Define a map of available flights where each key is a vector representing a route
; from a departure city to a destination city, and each value is a map with the price
; and number of connections for that flight.
(def available-flights
  {["Prague" "Vienna"] {:price 100 :connections 1}
   ["Prague" "Madrid"] {:price 100 :connections 1}
   ["Vienna" "Zadar"]  {:price 200 :connections 1}
   ["Zadar" "Kiev"]    {:price 200 :connections 2}
   ["Zadar" "Madrid"]  {:price 200 :connections 1}})

(defn get-all-flight-paths [departure destination]
  ;; todo this is not working at the moment. instead it has hardcoded lists;
  ;; todo use available-flights to generate all possible routes
  (cond (= ["Prague" "Zadar"] [departure destination])
        [{["Prague" "Vienna"] {:price 100 :connections 1}
          ["Vienna" "Zadar"]  {:price 200 :connections 1}}]

        (= ["Prague" "Madrid"] [departure destination])
        [{["Prague" "Madrid"] {:price 100 :connections 1}}
         {["Prague" "Vienna"] {:price 100 :connections 1}
          ["Vienna" "Zadar"]  {:price 200 :connections 1}
          ["Zadar" "Madrid"]  {:price 200 :connections 1}}]))

; Define the basic requirements for families and groups. Here, we use a map where the
; keys are the type of passenger group ('family' or 'group') and the values are maps
; that specify the maximum number of connections and the budget.
(def requirements
  {:family {:max-connections 2 :max-budget 700}
   :group  {:max-connections 3 :max-budget 1000}})

(defn route-price-and-connections [route]
  {:route-connections (->> route vals (map :connections) (reduce +))
   :route-price       (->> route vals (map :price) (reduce +))})

; Example function to check if a route meets the family or group requirements
(defn route-meets-requirements?
  [route-type route]
  (let [{:keys [max-connections max-budget]} (requirements route-type)
        {:keys [route-connections route-price]} (route-price-and-connections route)]
    (and (<= route-connections max-connections)
         (<= route-price max-budget))))

(defn find-flights
  "Returns a list of possible routes from `departure` to `destination` within the given `max-connections` and `max-price`."
  [departure destination type]
  (->> (get-all-flight-paths departure destination)
       (filter #(route-meets-requirements? type %))))

(defn format-travel-plans
  "Formats a list of travel plans for display."
  [travel-routes]
  ; Define a local function to format a single travel plan
  (let [format-plan (fn [route]
                      (let [{:keys [route-connections route-price]} (route-price-and-connections route)]
                        (prn "--------" route)
                        (str "Travel Plan:\n"
                             "  From: " (-> route first first first) "\n" ; Append the departure city
                             "  To: " (-> route first first second) "\n" ; Append the destination city
                             "  Total Price: $" route-price "\n" ; Append the total price
                             "  Total Connections: " route-connections "\n")))] ; Append the total number of connections
    ; Apply the format-plan function to each travel plan in the list and concatenate the results into a single string
    (apply str (map format-plan travel-routes))))

(defn parse-and-validate-input
  "Parses and validates user input. Expected input format: 'departure-city,destination-city,passenger-type'."
  [input-string]
  (let [[departure destination type] (clojure.string/split input-string #",")
        type (keyword type)]
    (println "[validate-input] departure =" departure "; destination =" destination "; type =" type)
    (cond
      (str/blank? departure)
      (do (println "Departure city is missing")
          false)

      (str/blank? destination)
      (do (println "Destination city is missing")
          false)

      (not (#{:family :group} type))
      (do
        (println "Group is missing")
        false)
      :else [departure destination type])))

;;-main function for launching from IDE also from command line
(defn -main
  "The main function that ties everything together in the travel application."
  [& [input]]
  (let [input-string (or input "Prague,Madrid,family")]
    ; Parse and validate the input
    (when-let [[departure destination type] (parse-and-validate-input input-string)]
      (let [; Find feasible travel plans based on the criteria
            travel-plans (find-flights departure destination type)
            formatted-plan (format-travel-plans travel-plans)]
        (prn formatted-plan)
        formatted-plan))))