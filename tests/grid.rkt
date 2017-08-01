#lang racket/base

(require rackunit)
(require granblue/grid)

(test-case
 "Check modifier<->percent"
 (check-= (modifier->percent 30) 13/10 0 "modifier->percent =")
 (check-= (percent->modifier 13/10) 30 0 "percent->modifier ="))

(test-case
 "Check weapon types"
 (check-= (unknown-large 1) 6.0 0 "Unknown large min =")
 (check-= (unknown-large 10) 15.0 0 "Unknown large med =")
 (check-= (unknown-large 15) 18.0 0 "Unknown large max =")
 (check-= (unknown-massive 1) 9.0 0 "Unknown massive min =")
 (check-= (unknown-massive 10) 18.0 0 "Unknown massive med =")
 (check-= (unknown-massive 15) 23.0 0 "Unknown massive max =")
 (check-= (magna-small 1) 1.0 0 "Magna small min =")
 (check-= (magna-small 10) 10.0 0 "Magna small med =")
 (check-= (magna-small 15) 12.0 0 "Magna small max =")
 (check-= (magna-medium 1) 3.0 0 "Magna medium min =")
 (check-= (magna-medium 10) 12.0 0 "Magna medium med =")
 (check-= (magna-medium 15) 14.5 0 "Magna medium max =")
 (check-= (magna-large 1) 6.0 0 "Magna large min =")
 (check-= (magna-large 10) 15.0 0 "Magna large med =")
 (check-= (magna-large 15) 18.0 0 "Magna large max =")
 (check-= (normal-small 1) 1.0 0 "Normal small min =")
 (check-= (normal-small 10) 10.0 0 "Normal small med =")
 (check-= (normal-small 15) 12.0 0 "Normal small max =")
 (check-= (normal-medium 1) 3.0 0 "Normal medium min =")
 (check-= (normal-medium 10) 12.0 0 "Normal medium med =")
 (check-= (normal-medium 15) 14.5 0 "Normal medium max =")
 (check-= (normal-large 1) 6.0 0 "Normal medium min =")
 (check-= (normal-large 10) 15.0 0 "Normal medium min =")
 (check-= (normal-large 15) 18.0 0 "Normal medium min =")
 (check-= (normal2-large 1) 7.0 0 "Normal medium min =")
 (check-= (normal2-large 10) 16.0 0 "Normal medium min =")
 (check-= (normal2-large 15) 20.0 0 "Normal medium min ="))
