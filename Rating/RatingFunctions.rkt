#lang racket
(require "../Team/TeamFunctions.rkt"
         "../Tournament/TournamentFunctions.rkt"
         "../Util.rkt")
(provide loadRatingData
         readRatingFile
         saveRating
         RatingData)

(define data #f)
(define RatingData #f)
(define RatingList #f)

(define out (open-output-file "Rating/RatingDatabase.txt" #:exists 'append))
(close-output-port out)

(define RatingCount 0)
(define (incrementRating)
  (set! RatingCount (add1 RatingCount)))

(define RatingWins 0)
(define (incrementWins)
  (set! RatingWins (add1 RatingWins)))

(define RatingLoses 0)
(define (incrementLoses)
  (set! RatingLoses (add1 RatingLoses)))

(define (readRatingFile)
  (set! data (file->list "Rating/RatingDatabase.txt"))
  (set! RatingData (list (map (lambda (list)
                                (car list))
                              data)
                         (map (lambda (list)
                                (cadr list))
                              data)
                         (map (lambda (list)
                                (number->string(caddr list)))
                              data)
                         (map (lambda (list)
                                (number->string(cadddr list)))
                              data))))

(define (countRating team list1 list2)
  (for ([i list1])
    (if (string=? team i) (incrementWins) #f))

  (for ([i list2])
    (if (string=? team i) (incrementLoses) #f)))

(define (loadRatingData table)
  (readRatingFile)
  (send table set
        (list-ref RatingData 0)
        (list-ref RatingData 1)
        (list-ref RatingData 2)
        (list-ref RatingData 3)))
  
(define (saveRating)
  (display-lines-to-file '() "Rating/RatingDatabase.txt" #:exists 'truncate)
  (set! RatingCount 0)
  (readTeamFile)
  (readTournamentFile)
  (joinIdAndName (list-ref TeamData 0) (list-ref TeamData 1))
  (set! RatingList (map (lambda (el)
                          (set! RatingWins 0)
                          (set! RatingLoses 0)
                          (countRating el (list-ref TournamentData 6) (list-ref TournamentData 7))
                          (list el RatingWins RatingLoses))
                        IdAndName))
  
  (define SortedList
    (sort RatingList #:key cadr >))
  
  (define (AddId id el)
    (append (list(number->string id)) el))
  
  (for ([el SortedList])
    (incrementRating)
    (write-to-file (AddId RatingCount el) "Rating/RatingDatabase.txt" #:exists 'append #:mode 'text)))