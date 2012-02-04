(ns taggp.examples.phygp
  (:use [taggp.core :exclude [-main evolve]]))

(def closing-data
[["a"
  [0
   0.0
   0.0
   0.0
   1.0401669671716806E-6
   2.146997145052943E-5
   1.2973453581081957E-4
   4.635358001739091E-4
   0.001226655514134775
   0.002662102571644529
   0.005004997392081073
   0.008423427286338523
   0.012959121577819113
   0.01848137257259229
   0.024666603624151497
   0.03101223335944633
   0.036887437889746806
   0.04161606916113452
   0.044579777423880765
   0.04532389424946391
   0.04364623369131148
   0.03965046126038532
   0.03375103010950551
   0.026624969128269393
   0.01911539452387039
   0.012100534801439149
   0.006348477575796007
   0.0023805124801256545
   3.6446299247844136E-4
   5.428154998849136E-5
   7.84655254209011E-4
   0.0015210295263266146
   9.578637625031765E-4
   -0.0023477325611022124
   -0.00982296929548676
   -0.02272354040678585
   -0.04199012892613331
   -0.06812965040674845
   -0.10113036400867871
   -0.14041680340327176
   -0.18484773167764426
   -0.23275804024797717
   -0.28204348373207655
   -0.3302850381749679
   -0.37490717770200266
   -0.4133613373292898
   -0.443322392003469
   -0.46288257606423505
   -0.4707246146756387
   -0.46625481100345206
   -0.4496782440629876
   -0.4220025896306269
   -0.3849643514535157
   -0.34088077873641887
   -0.29244108762276855
   -0.24245998392073778
   -0.1936230000250551
   -0.14825527690506138
   -0.10814239621831516
   -0.07442399905889922
   -0.04756956964285588
   -0.027433049050907782
   -0.013371308332080348
   -0.004403114764665786
   6.18529233348393E-4
   0.002846554037484816
   0.003312842981363693
   0.002846132890374906
   0.0020376230115836647
   0.0012488690080360127
   6.506200287924599E-4
   2.783890742261869E-4
   9.074851909650504E-5
   1.905383841075397E-5
   1.594779241320113E-6
   2.297989554150602E-9
   1.265589949113598E-7
   2.7908853757627174E-6
   1.135902298542659E-5
   2.2786199756322006E-5
   2.6920091441652457E-5
   1.5706774422907436E-5
   2.8579494164504544E-7
   3.051371388672155E-5
   2.0905409574857638E-4
   6.915205634938734E-4
   0.001665692866340551
   0.003306557243221008
   0.005709352866780409
   0.008808939107771656
   0.01229928746215787
   0.01557032643606603
   0.01767958244598416
   0.01737253168451368
   0.013158604297590802
   0.0034405092921506422
   -0.013315240856093015
   -0.03838701543733436
   -0.07261139281867267
   -0.11622724172956117]]
 ["b"
  [0
   0.0
   0.0
   0.0018761084431322102
   0.007425189324690572
   0.01645004986334156
   0.02865199515610389
   0.043636005270759164
   0.06091791034570269
   0.079933789908125
   0.10005178539617662
   0.12058641944274212
   0.14081536604963524
   0.1599984240542985
   0.1773982307731398
   0.19230203709053664
   0.20404367595217512
   0.21202471919236054
   0.21573375500392886
   0.21476274518336647
   0.20881954296322183
   0.19773586319206812
   0.1814702803601438
   0.1601061607839105
   0.13384478123435775
   0.10299421324954955
   0.06795482817044071
   0.029202476413800242
   -0.012729501536677202
   -0.057268272025591406
   -0.10381844651799232
   -0.1517770717912239
   -0.20054602173149624
   -0.24954141680566513
   -0.2981999846552606
   -0.34598254909782816
   -0.39237507420357154
   -0.43688787897266307
   -0.47905376567201713
   -0.51842586538164
   -0.5545759965478317
   -0.5870942589796132
   -0.61559045264671
   -0.6396977266159378
   -0.6590786399988413
   -0.673433568064542
   -0.6825111294684582
   -0.6861200638384696
   -0.6841417731043085
   -0.6765425753163395
   -0.663384624744604
   -0.6448344411098001
   -0.6211680717969554
   -0.5927720834528815
   -0.5601398336691947
   -0.5238627903265959
   -0.4846170184365432
   -0.4431453094230298
   -0.4002357515057016
   -0.35669780041148574
   -0.31333708179547115
   -0.2709302249965622
   -0.2302009883031033
   -0.19179879699911287
   -0.15628059602062108
   -0.12409664589864741
   -0.09558059481538835
   -0.07094387219943826
   -0.05027419782362625
   -0.0335378056744122
   -0.02058485622213117
   -0.011157457246281746
   -0.004899726526787893
   -0.0013693968056086043
   -5.056670998713099E-5
   -3.673204390610988E-4
   -0.0016980533921435962
   -0.003390432050031458
   -0.004776969729740176
   -0.005191206104460636
   -0.003984434705069376
   -5.428327862380904E-4
   0.005695277757631118
   0.015222455328213225
   0.028446011596659302
   0.04567341734769085
   0.06710000411726567
   0.09279973938712567
   0.12271979197309257
   0.1566794626894771
   0.19437383860195453
   0.23538225230415277
   0.2791813145226894
   0.3251619690826182
   0.37264972666552326
   0.4209269992509667
   0.46925630650577804
   0.5169030755878185
   0.5631568126390724
   0.6073495816732953]]
 ["c"
  [0
   0.0
   0.0434645639093957
   0.0868469771243281
   0.13006524422019045
   0.17303768001861927
   0.21568306397749717
   0.25792079370276405
   0.29967103729189076
   0.34085488422107674
   0.3813944944909848
   0.42121324574911884
   0.4602358781107752
   0.4983886364048455
   0.535599409575618
   0.5717978669771013
   0.6069155913022625
   0.6408862078959389
   0.6736455102070115
   0.7051315811427286
   0.735284910095813
   0.7640485054231633
   0.7913680021635592
   0.8171917647907752
   0.8414709848078965
   0.8641597729983743
   0.885215246159463
   0.9045976081541067
   0.9222702251280795
   0.9381996947502229
   0.9523559093448997
   0.9647121127973508
   0.9752449511243938
   0.9839345166148865
   0.9907643854565227
   0.9957216487778453
   0.998796937046807
   0.9999844377797601
   0.9992819065274081
   0.9966906711169526
   0.9922156291424185
   0.9858652387078994
   0.9776515024412241
   0.9675899448082503
   0.9556995827706725
   0.9420028898427959
   0.9265257536152162
   0.9092974268256817
   0.8903504720696241
   0.8697207002548539
   0.8474471029167449
   0.82357177852184
   0.7981398528991634
   0.7711993939496329
   0.7428013207947759
   0.7129993075364653
   0.6818496818095685
   0.6494113183192606
   0.6157455275642169
   0.5809159399560041
   0.544988385553701
   0.5080307696410375
   0.47011294438124407
   0.4313065767921727
   0.3916850132912326
   0.3513231410661608
   0.3102972465336348
   0.2686848711532635
   0.22656466486946822
   0.18401623745825907
   0.1411200080598672
   0.09795705318163506
   0.05460895345843852
   0.011157639460277329
   -0.03231476316152965
   -0.07572608889687138
   -0.11899428767470273
   -0.16203757994271653
   -0.20477461123571558
   -0.24712460594055208
   -0.28900751996699264
   -0.33034419203596427
   -0.3710564932992334
   -0.4110674750077213
   -0.4503015139493715
   -0.4886844553816593
   -0.5261437531886088
   -0.5626086069974077
   -0.5980100959954554
   -0.6322813091949354
   -0.6653574718986879
   -0.6971760681283639
   -0.7276769587834632
   -0.7568024953079282
   -0.7844976286494536
   -0.8107100133055735
   -0.8353901062598788
   -0.8584912606213543
   -0.879969813789874
   -0.89978516998119]]]

  )

(defn avg [& nums] (/ (reduce + nums) (count nums)))

(def averaged-closing-data
  (vec (apply (partial map avg) (map second closing-data))))

(def target-data (atom [[{'yesterday 0 'lastweek 0 'avyesterday 0 'avlastweek 0} 0]]))

(defn sin [n] (Math/sin n))

(defn cos [n] (Math/cos n))

(defn noop0 [] 0)
  
(defn noop1 [a] a)

(defn error
  "Error function for testing individuals."
  [individual]
  (reduce +' (map (fn [[bindings target]] 
                    (let [result (eval-with-tagging individual @execution-limit bindings 0)]
                      (if (= result :limit-exceeded) 
                        @penalty-for-exceeding-limit
                        (abs (-' result target)))))
                  @target-data)))

(defn set-target-from-generation
  [g]
  (reset! target-data 
          (let [my-closings (second (first closing-data))] ;; only care about first guy, except for averages
            (for [day (range 10)]
              [{'lastweek (get my-closings (+ g day))
                'yesterday (get my-closings (+ 4 g day))
                'avlastweek (get averaged-closing-data (+ g day))
                'avyesterday (get averaged-closing-data (+ 4 g day))}
               (get my-closings (+ 5 g day))]))))

(def cumulative-best-error-past-gen50 (atom 0))
(def cumulative-median-error-past-gen50 (atom 0))

(defn evolve []
  (println "allow-tagging =" @allow-tagging)
  (println "tagdo-semantics =" @tagdo-semantics)
  (println "use-noops =" @use-noops)
  (println "trivial-geography-radius =" @trivial-geography-radius)
  (println "population-size =" @population-size)
  (println "max-generations =" @maximum-generations)  
  (update-terminal-proportion)
  (println "Starting evolution...")
  (loop [generation 0
         population (pair-with-errors 
                      (concat (repeatedly (/ @population-size 2) #(random-code (ramp-depth) :grow))
                              (repeatedly (/ @population-size 2) #(random-code (ramp-depth) :full))))]
    (let [sorted (sort #(< (second %1) (second %2)) population)]
      (println "Generation:" generation)
      (println "Best error:" (second (first sorted)))
      (println "Best program:" (first (first sorted)))
      (println "Best program size:" (codesize (first (first sorted))))
      (println "Best program depth:" (depth (first (first sorted))))
      (println "     Median error:" (second (nth sorted 
                                                 (int (/ @population-size 2)))))
      (println "     Average program size:" 
               (float (/ (reduce + (map codesize (map first population)))
                         (count population))))
      (println "     Average program depth:" 
               (float (/ (reduce + (map depth (map first population)))
                         (count population))))
      (println "     Tag call ratio:"
               (float (/ (count (filter :tag (filter map? (flatten (map first population)))))
                         (count (flatten (map first population))))))
      (println "     Tagged call ratio:"
               (float (/ (count (filter :tagged (filter map? (flatten (map first population)))))
                         (count (flatten (map first population))))))
      (println "     Tagged-with-args call ratio:"
               (float (/ (count (filter :tagged-with-args (filter map? (flatten (map first population)))))
                         (count (flatten (map first population))))))
      (println "     Unique error values in population:"
               (count (distinct (map second population))))
      (when (> generation 50)
        (swap! cumulative-median-error-past-gen50 #(+ % (second (nth sorted (int (/ @population-size 2))))))
        (swap! cumulative-best-error-past-gen50 #(+ % (second (first sorted)))))
      (if (>= generation (- (apply min (map count (map second closing-data))) 16))  
        (do (println "Cumulative best error after gen 50:" @cumulative-best-error-past-gen50)
            (println "Cumulative median error after gen 50:" @cumulative-median-error-past-gen50))
        (do (flush)
            (set-target-from-generation (inc generation))
            (recur 
              (inc generation)
              (pair-with-errors
                (for [i (range @population-size)]
                  (let [operator (rand)
                        tsize @reproductive-tournament-size
                        radius @trivial-geography-radius]
                    (cond (< operator 
                             @mutation-fraction)      (mutate (select population tsize i radius))
                          (< operator 
                             (+ @mutation-fraction 
                                @crossover-fraction)) (crossover (select population tsize i radius)
                                                                 (select population tsize i radius))
                          :else (select population tsize i radius)))))))))))


(defn run
  "An evolutionary run. This is separated for REPL usage."
  []
  (reset! allow-tagging true)
  (reset! function-table
          (if @allow-tagging
            (zipmap '(+' -' *' pd sin cos :tagged-erf :tag-erf :tagged-with-args-erf)
                    '(2  2  2  2  1   1   0           1         2))
            (if @use-noops
              (zipmap '(+' -' *' pd sin cos noop0 noop1 noop1)
                      '(2  2  2  2  1   1   0     1     1))
              (zipmap '(+' -' *' pd sin cos)
                      '(2  2  2  2  1   1  )))))
  (reset! terminal-set
          (if @allow-tagging
            '(yesterday lastweek avyesterday avlastweek :float-erc arg0 arg1)
            '(yesterday lastweek avyesterday avlastweek :float-erc)))
  (reset! ramp-depth-range [4 7])
  (reset! population-size 1000)
  (reset! error-fn error)       
  (evolve))

(defn -main 
  [& params]
  (in-ns 'taggp.examples.phygp) ;; when using lein run (= *ns* 'user) by default, we need to switch
  (let [params (merge {:allow-tagging true
                       :tagdo-semantics true
                       :use-noops true}
                      (apply hash-map (map read-string params)))]
    (println "target-data =" target-data)
    (reset! allow-tagging (:allow-tagging params))
    (reset! tagdo-semantics (:tagdo-semantics params))
    (reset! use-noops (:use-noops params))
    (run)
    (System/exit 0)))

(run)
