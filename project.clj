(defproject fct "0.5.2"
  :description "Free variables in Clojure. We provide a framework for global and free variables in Clojure. Global means they have a global meaning (unlike variables in a function declaration) and free means that they are undefined. This turns any code into a function that can be evaluated by assigning interpretations to the variables."
  :url "http://github.com/achatz64/fct"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]])
