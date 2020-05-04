#lang racket
(require "Util.rkt")
(provide loadTournamentData
         saveTournament
         editTournament
         deleteTournament
         TournamentData
         readTournamentFile
         FillFirstChoise
         FillSecondChoise
         isEditTournament
         setIsEditTournament
         SelectedTournament
         getSelectedTournament)

(define data #f)
(define TournamentData #f)
(define SelectedTournament #f)

(define isEditTournament #f)
(define (setIsEditTournament x)
  (set! isEditTournament x))

(define TournamentCount 0)
(define (incrementTournament)
  (set! TournamentCount (add1 TournamentCount)))

(define out (open-output-file "TournamentDatabase.txt" #:exists 'append))
(close-output-port out)

(define (readTournamentFile)
  (set! data (file->list "TournamentDatabase.txt"))
  (set! TournamentData (list
                        (map (lambda (list)
                               (car list))
                             data)
                        (map (lambda (list)
                               (cadr list))
                             data)
                        (map (lambda (list)
                               (caddr list))
                             data)
                        (map (lambda (list)
                               (cadddr list))
                             data)
                        (map (lambda (list)
                               (cadddr (cdr list)))
                             data)
                        (map (lambda (list)
                               (cadddr (cddr list)))
                             data)
                        (map (lambda (list)
                               (cadddr (cdddr list)))
                             data)
                        (map (lambda (list)
                               (cadddr (cddddr list)))
                             data))))

(define (FillFirstChoise teams choice)
  (map (lambda (el)
         (send choice append el))
       teams))

(define (FillSecondChoise teams choice)
  (map (lambda (el)
         (send choice append el))
       teams))

(define (getSelectedTournament id)
  (readTournamentFile)
  (map (lambda (list)
         (if (equal? (car list) (number->string (+ id 1))) (set! SelectedTournament list) #f))
       data))

(define (loadTournamentData table)
  (readTournamentFile)
  (send table set
        (list-ref TournamentData 0)
        (list-ref TournamentData 1)
        (list-ref TournamentData 2)
        (list-ref TournamentData 3)
        (list-ref TournamentData 4)
        (list-ref TournamentData 5)
        (list-ref TournamentData 6)
        (list-ref TournamentData 7)))

(define (saveTournament Team1 Team2 Date Team1Score Team2Score)
  (readTournamentFile)
  (cond
    [(= (length data) 0) (incrementTournament)]
    [else (set! TournamentCount (add1(string->number(last (list-ref TournamentData 0)))))])
  (write-to-file (list
                  (number->string TournamentCount)
                  Team1
                  Team2
                  Date
                  Team1Score
                  Team2Score
                  (cond
                    [(> (string->number Team1Score) (string->number Team2Score)) Team1]
                    [(<(string->number Team1Score) (string->number Team2Score)) Team2]
                    [else "Ничья"])
                  (cond
                    [(< (string->number Team1Score) (string->number Team2Score)) Team1]
                    [(> (string->number Team1Score) (string->number Team2Score)) Team2]
                    [else "Ничья"]))
                 "TournamentDatabase.txt" #:exists 'append #:mode 'text))

(define (editTournament id Team1 Team2 Date Team1Score Team2Score)
  (readTournamentFile)
  (getSelectedTournament id)
  (display-lines-to-file '() "TournamentDatabase.txt" #:exists 'truncate)
  (set! isEditTournament #f)
  (map (lambda (list)
         (write-to-file list "TournamentDatabase.txt" #:exists 'append #:mode 'text))
       (list-set data (- (string->number(car SelectedTournament)) 1) (list
                                                                      (car SelectedTournament)
                                                                      Team1
                                                                      Team2
                                                                      Date
                                                                      Team1Score
                                                                      Team2Score
                                                                      (cond
                                                                        [(> (string->number Team1Score) (string->number Team2Score)) Team1]
                                                                        [(<(string->number Team1Score) (string->number Team2Score)) Team2]
                                                                        [else "Ничья"])
                                                                      (cond
                                                                        [(< (string->number Team1Score) (string->number Team2Score)) Team1]
                                                                        [(> (string->number Team1Score) (string->number Team2Score)) Team2]
                                                                        [else "Ничья"])))))

(define (deleteTournament id)
  (readTournamentFile)
  (display-lines-to-file '() "TournamentDatabase.txt" #:exists 'truncate)
  (map (lambda (list)
         (write-to-file list "TournamentDatabase.txt" #:exists 'append #:mode 'text))
       (remove-nth data id)))