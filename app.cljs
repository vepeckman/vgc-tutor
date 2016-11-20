#!/usr/bin/env lumo
(ns vgc-tutor.app
  (:require [clojure.string :as string]
            [cljs.nodejs :as nodejs]))

(def pokedex   (get (js->clj (nodejs/require "./pokedex"))   "BattlePokedex"))
(def learnsets (get (js->clj (nodejs/require "./learnsets")) "BattleLearnsets"))
(def moves     (get (js->clj (nodejs/require "./moves"))     "BattleMovedex"))
(def abilities (get (js->clj (nodejs/require "./abilities")) "BattleAbilities"))
(def fs (nodejs/require "fs"))
(def legal-mons (string/split (. fs readFileSync "legal-mons.txt" "utf8") #"\r\n"))

(defn find-flags
  []
  (let [argv (aget nodejs/process "argv")]
    (for [x (range (count argv))
          :let [arg (aget argv x)
                n   (aget argv (inc x))]
          :when (string/starts-with? arg "--")]
      [(keyword (string/replace arg "-" "")) n])))

(defn aggregate-flags
  []
  (reduce #(assoc %1 (first %2) (last %2)) {} (find-flags)))

(defn find-poke-info
  [poke]
  (filter #(or (= (get (last %) "species") poke) (= (get (last %) "baseSpecies") poke)) pokedex))

(defn find-move-info
  [move-id]
  (select-keys (get moves move-id) ["name" "accuracy" "basePower" "category" "type" "target" "priority" "secondary" "flags" "zMovePower"]))

(defn find-learnset
  [poke-id]
  (map find-move-info (keys (get (get learnsets poke-id) "learnset"))))

(defn find-ability-info
  [ability]
  (select-keys (last (first (filter #(= (get (last %) "name") ability) abilities))) ["shortDesc"]))

(defn rand-mon
  []
  (rand-nth legal-mons))

(defn final-evo?
  [poke-info]
  (not (get poke-info "evos")))

(defn add-learnset
  [info-map poke-id]
  (assoc info-map "learnset" (find-learnset poke-id)))

(defn full-info
  [mon]
  (let [pokes (find-poke-info mon)
        poke-ids (map first pokes)
        poke-infos (map last pokes)]
      (for [x (range (count pokes))
            :let [poke-id (nth poke-ids x)
                  poke-info (nth poke-infos x)]]
        (add-learnset (select-keys poke-info ["species" "baseStats" "abilities" "types"]) poke-id))))

(defn random-evo-info
  []
  (full-info (rand-mon)))

(defn print-move
  [move-info]
  (println (str "Name: " (get move-info "name")))
  (println (str "  Type: " (get move-info "type")))
  (println (str "  Base Power: " (get move-info "basePower"))))

(defn print-poke-info
  [info-map]
  (println (str "Name: " (get info-map "species")))
  (println (str "Type: " (get info-map "types")))
  (println (str "Stats: " (get info-map "baseStats")))
  (println (str "Abilities: " (get info-map "abilities"))))

(defn flag-command
  [flag option]
  (cond
    (and (= :random flag) option) (doseq [info-map (random-evo-info)]
                                     (print-poke-info info-map))
    (and (= :move flag) option) (print-move (find-move-info option))
    (and (= :learnset flag) option) (doseq [move (find-learnset option)]
                                      (print-move move))
    (and (= :ability flag) option) (println (find-ability-info option))
    (and (= :dex flag) option) (doseq [info-map (full-info option)]
                                 (print-poke-info info-map))))

(let [flags (aggregate-flags)]
  (doseq [flag (keys flags)]
    (flag-command flag (flag flags))))