#lang racket
(require "../Util.rkt")
(provide loadTeamData
         saveTeam
         editTeam
         deleteTeam
         TeamData
         data
         readTeamFile
         getSelectedTeam
         SelectedTeam
         isEditTeam
         setIsEditTeam)

(define data #f)
(define TeamData #f)
(define SelectedTeam #f)

(define isEditTeam #f)
(define (setIsEditTeam x)
  (set! isEditTeam x))

(define TeamCount 0)
(define (incrementTeam)
  (set! TeamCount (add1 TeamCount)))

(define out (open-output-file "Team/TeamDatabase.txt" #:exists 'append))
(close-output-port out)

(define (readTeamFile)
  (set! data (file->list "Team/TeamDatabase.txt"))
  (set! TeamData (list (map (lambda (list)
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
                            data))))

(define (loadTeamData table)
  (readTeamFile)
  (send table set(list-ref TeamData 0) (list-ref TeamData 1) (list-ref TeamData 2)(list-ref TeamData 3)))

(define (saveTeam Name City Coach)
  (readTeamFile)
  (cond
    [(= (length data) 0) (set! TeamCount 1)]
    [else (set! TeamCount (add1(string->number(last (list-ref TeamData 0)))))])
  (write-to-file (list
                  (number->string TeamCount)
                  Name
                  City
                  Coach)
                 "Team/TeamDatabase.txt" #:exists 'append #:mode 'text))

(define (getSelectedTeam id)
  (readTeamFile)
  (map (lambda (list)
         (if (equal? (car list) (number->string (+ id 1))) (set! SelectedTeam list) #f))
       data))

(define (editTeam id Name City Coach)
  (readTeamFile)
  (getSelectedTeam id)
  (display-lines-to-file '() "Team/TeamDatabase.txt" #:exists 'truncate)
  (set! isEditTeam #f)
  (map (lambda (list)
         (write-to-file list "Team/TeamDatabase.txt" #:exists 'append #:mode 'text))
       (list-set data (- (string->number(car SelectedTeam)) 1) (list (car SelectedTeam) Name City Coach))))

(define (deleteTeam id)
  (readTeamFile)
  (display-lines-to-file '() "Team/TeamDatabase.txt" #:exists 'truncate)
  (map (lambda (list)
         (write-to-file list "Team/TeamDatabase.txt" #:exists 'append #:mode 'text))
       (remove-nth data id)))
        
