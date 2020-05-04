#lang racket
(require
  racket/gui/base
  racket/class
  racket/list
  "Team/TeamFunctions.rkt"
  "Tournament/TournamentFunctions.rkt"
  "Rating/RatingFunctions.rkt"
  "Util.rkt")

(define BasketballManagerGUI #f)

; Объявление окна добавления команды
(define AddTeamDialog #f)
(define TeamNameField #f)
(define TeamCityField #f)
(define TeamCoachField #f)

; Объявление интерфейса таблиц (командная, турнирная, рейтинговая)
(define TeamTable #f)
(define TournamentTable #f)
(define RatingTable #f)

;Объявление окна добавления матча
(define AddTournamentDialog #f)
(define ChooseFirstTeam #f)
(define ChooseSecondTeam #f)
(define TournamentDate #f)
(define Team1Score #f)
(define Team2Score #f)
(define SelectedTeam1 #f)
(define SelectedTeam2 #f)

(define SelectedTab 0)

(define WarningDialog #f)
(define WarningMessage #f)

(define ConfirmDialog #f)
(define ConfirmMessage #f)

(define (BasketballManagerGUI-init
         
         ;При нажатии на кнопку "Добавить команду" открывается окно добавления команды
         (AddTeamButton-callback (lambda (button control-event) (list
                                                                 (send AddTeamDialog set-label "Добавление команды")
                                                                 (showDialog AddTeamDialog))))
         
         ;При нажатии на кнопку "Принять" добавляется новая команда
         (AddTeamAccept-callback (lambda (button control-event) (list
                                                                 ;Если добавляем команду, то вызываем saveTeam, если редактируем, то editTeam
                                                                 (if (false? isEditTeam)
                                                                     (saveTeam
                                                                      ;Собираем введенные значения из текстовых полей
                                                                      (send TeamNameField get-value)
                                                                      (send TeamCityField get-value)
                                                                      (send TeamCoachField get-value))  
                                                                     (editTeam
                                                                      (last(send TeamTable get-selections))
                                                                      ;Собираем введенные значения из текстовых полей
                                                                      (send TeamNameField get-value)
                                                                      (send TeamCityField get-value)
                                                                      (send TeamCoachField get-value)))
                                                                 ;Очищаем текстовые поля
                                                                 (clearFields TeamNameField TeamCityField TeamCoachField)
                                                                 ;Закрываем окно добавления команды
                                                                 (closeDialog AddTeamDialog)
                                                                 ;Загружаем список команд чтобы отобразить только что добавленную
                                                                 ;Реализация данного метода находится в файле TeamFunctions.rkt
                                                                 (loadTeamData TeamTable))))
         
         ;При нажатии на кнопку "Отмена" очищаются текстовые поля и закрывается окно добавления команды
         (AddTeamCancel-callback (lambda (button control-event) (list(clearFields TeamNameField TeamCityField TeamCoachField)
                                                                     (closeDialog AddTeamDialog))))
         
         (EditTeamButton-callback (lambda (button control-event) (if (empty? (send TeamTable get-selections))
                                                                     (list
                                                                      (send WarningMessage set-label "Вы не выбрали команду")
                                                                      (showDialog WarningDialog))
                                                                     (list
                                                                      (setIsEditTeam #t)
                                                                      (getSelectedTeam (last(send TeamTable get-selections)))
                                                                      (send TeamNameField set-value (cadr SelectedTeam))
                                                                      (send TeamCityField set-value (caddr SelectedTeam))
                                                                      (send TeamCoachField set-value (cadddr SelectedTeam))
                                                                      (send AddTeamDialog set-label "Изменение команды")
                                                                      (showDialog AddTeamDialog)))))
         
         (DeleteTeamButton-callback (lambda (button control-event) (if (empty? (send TeamTable get-selections))
                                                                       (list
                                                                        (send WarningMessage set-label "Вы не выбрали команду")
                                                                        (showDialog WarningDialog))
                                                                       (list
                                                                        (send ConfirmMessage set-label "Вы уверены что хотите удалить команду?")
                                                                        (showDialog ConfirmDialog)))))
         
         (AddTournamentButton-callback (lambda (button control-event) (list
                                                                       (readTeamFile)
                                                                       (if (>= (length data) 2)
                                                                           (list
                                                                            (send ChooseFirstTeam clear)
                                                                            (send ChooseSecondTeam clear)
                                                                            (joinIdAndName (list-ref TeamData 0) (list-ref TeamData 1))
                                                                            (FillFirstChoise (remove (second IdAndName) IdAndName) ChooseFirstTeam)
                                                                            (FillSecondChoise (remove (send ChooseFirstTeam get-string-selection) IdAndName) ChooseSecondTeam)
                                                                            (showDialog AddTournamentDialog))
                                                                           (list
                                                                            (send WarningMessage set-label "Недостаточно команд для проведения турнира")
                                                                            (showDialog WarningDialog))))))
         
         (ChooseFirstTeam-callback (lambda (choice control-event) (list
                                                                   (set! SelectedTeam2 (send ChooseSecondTeam get-string-selection))
                                                                   (send ChooseSecondTeam clear)
                                                                   (joinIdAndName (list-ref TeamData 0) (list-ref TeamData 1))
                                                                   (FillSecondChoise (remove (send ChooseFirstTeam get-string-selection) IdAndName) ChooseSecondTeam)
                                                                   (send ChooseSecondTeam set-string-selection SelectedTeam2))))
         
         (ChooseSecondTeam-callback (lambda (choice control-event) (list
                                                                    (set! SelectedTeam1 (send ChooseFirstTeam get-string-selection))
                                                                    (send ChooseFirstTeam clear)
                                                                    (joinIdAndName (list-ref TeamData 0) (list-ref TeamData 1))
                                                                    (FillFirstChoise (remove (send ChooseSecondTeam get-string-selection) IdAndName) ChooseFirstTeam)
                                                                    (send ChooseFirstTeam set-string-selection SelectedTeam1))))
         
         (AddTournamentAccept-callback (lambda (button control-event)(list
                                                                      ;Если добавляем команду, то вызываем saveTeam, если редактируем, то editTeam
                                                                      (if (false? isEditTournament)
                                                                          (saveTournament
                                                                           (send ChooseFirstTeam get-string-selection)
                                                                           (send ChooseSecondTeam get-string-selection)
                                                                           (send TournamentDate get-value)
                                                                           (send Team1Score get-value)
                                                                           (send Team2Score get-value))
                                                                          (editTournament
                                                                           (last(send TournamentTable get-selections))
                                                                           ;Собираем введенные значения из текстовых полей
                                                                           (send ChooseFirstTeam get-string-selection)
                                                                           (send ChooseSecondTeam get-string-selection)
                                                                           (send TournamentDate get-value)
                                                                           (send Team1Score get-value)
                                                                           (send Team2Score get-value)))
                                                                      ;Очищаем текстовые поля
                                                                      (clearFields TournamentDate Team1Score Team2Score)
                                                                      ;Закрываем окно добавления команды
                                                                      (closeDialog AddTournamentDialog)
                                                                      ;Загружаем список команд чтобы отобразить только что добавленную
                                                                      ;Реализация данного метода находится в файле TeamFunctions.rkt
                                                                      (loadTournamentData TournamentTable))))
         
         (AddTournamentCancel-callback (lambda (button control-event) (list(clearFields TournamentDate Team1Score Team2Score)
                                                                           (closeDialog AddTournamentDialog))))
         
         (EditTournamentButton-callback (lambda (button control-event) (if (empty? (send TournamentTable get-selections))
                                                                           (list
                                                                            (send WarningMessage set-label "Вы не выбрали матч")
                                                                            (showDialog WarningDialog))
                                                                           (list
                                                                            (setIsEditTournament #t)
                                                                            (getSelectedTournament (last(send TournamentTable get-selections)))
                                                                            (joinIdAndName (list-ref TeamData 0) (list-ref TeamData 1))
                                                                            (FillFirstChoise (remove (second IdAndName) IdAndName) ChooseFirstTeam)
                                                                            (FillSecondChoise (remove (send ChooseFirstTeam get-string-selection) IdAndName) ChooseSecondTeam)
                                                                            (send ChooseFirstTeam set-string-selection (cadr SelectedTournament))
                                                                            (send ChooseSecondTeam set-string-selection (caddr SelectedTournament))
                                                                            (send TournamentDate set-value (cadddr SelectedTournament))
                                                                            (send Team1Score set-value (cadddr (cdr SelectedTournament)))
                                                                            (send Team2Score set-value (cadddr(cddr SelectedTournament)))
                                                                            (send AddTournamentDialog set-label "Изменение матча")
                                                                            (showDialog AddTournamentDialog)))))
         
         (DeleteTournamentButton-callback (lambda (button control-event) (if (empty? (send TournamentTable get-selections))
                                                                             (list
                                                                              (send WarningMessage set-label "Вы не выбрали матч")
                                                                              (showDialog WarningDialog))
                                                                             (list
                                                                              (send ConfirmMessage set-label "Вы уверены что хотите удалить матч?")
                                                                              (showDialog ConfirmDialog)))))
         
         (ReloadRatingButton-callback (lambda (button control-event) (list
                                                                      (saveRating)
                                                                      (loadRatingData RatingTable))))
         
         (DeleteConfirmAccept-callback (lambda (button control-event) (cond 
                                                                        [(equal? SelectedTab 0)
                                                                         (deleteTeam (last(send TeamTable get-selections)))
                                                                         (closeDialog ConfirmDialog)
                                                                         (loadTeamData TeamTable)]
                                                                        [(equal? SelectedTab 1)
                                                                         (deleteTournament (last(send TournamentTable get-selections)))
                                                                         (closeDialog ConfirmDialog)
                                                                         (loadTournamentData TournamentTable)])))
         
         (WarningOk-callback (lambda (button control-event) (closeDialog WarningDialog)))
         
         (DeleteConfirmCancel-callback (lambda (button control-event) (closeDialog ConfirmDialog))))
  
  (define MainFrame
    (new
     frame%
     (parent BasketballManagerGUI)
     (label "Баскетбол  Менеджер")
     (width 800)
     (height 600)
     (alignment (list 'center 'top))
     (stretchable-width #t)
     (stretchable-height #t)))
  (define TabPanel
    (new
     (class tab-panel%
       (super-new)
       (define child-panels '())
       (define/public
         (add-child-panel p label)
         (set! child-panels (append child-panels (list p)))
         (send this append label)
         (when (> (length child-panels) 1) (send this delete-child p)))
       (define/public
         (active-child n)
         (send this change-children
               (lambda (children) (list (list-ref child-panels n))))))
     (parent MainFrame)
     (choices (list))
     (callback (λ (tp e) (list
                          (send tp active-child (send tp get-selection))
                          (set! SelectedTab (send tp get-selection)))))
     (stretchable-width #t)
     (stretchable-height #t)))
  (define TeamTab
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent parent))
       (send parent add-child-panel this label))
     (parent TabPanel)
     (label "Команды")
     (alignment (list 'left 'center))))
  
  (define TeamOptionsBox
    (new
     group-box-panel%
     (parent TeamTab)
     (label "Возможности")
     (stretchable-width #t)
     (stretchable-height #f)))
  (define TeamOptionsPanel
    (new
     horizontal-pane%
     (parent TeamOptionsBox)
     (spacing 100)
     (alignment (list 'center 'center))))
  (define AddTeamButton
    (new
     button%
     (parent TeamOptionsPanel)
     (label "Добавить команду")
     (callback AddTeamButton-callback)))
  (define EditTeamButton
    (new
     button%
     (parent TeamOptionsPanel)
     (label "Изменить команду")
     (callback EditTeamButton-callback)))
  (define DeleteTeamButton
    (new
     button%
     (parent TeamOptionsPanel)
     (label "Удалить команду")
     (callback DeleteTeamButton-callback)))
  (define TeamBox
    (new
     group-box-panel%
     (parent TeamTab)
     (label "Команды")
     (alignment (list 'center 'top))))

  (set! TeamTable (new list-box%
                       [parent TeamBox]
                       [choices (list )]
                       [label ""]
                       [style (list 'single 'column-headers 'variable-columns)]
                       [columns (list "Id" "Название" "Город" "Тренер")]))
  (loadTeamData TeamTable)
  (define TournamentTab
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent parent))
       (send parent add-child-panel this label))
     (parent TabPanel)
     (label "Соревнования")
     (alignment (list 'left 'center))))
  (define TournamentOptionsPanel
    (new
     group-box-panel%
     (parent TournamentTab)
     (label "Возможности")
     (stretchable-width #t)
     (stretchable-height #f)))
  (define TournamentOptionsBox
    (new
     horizontal-pane%
     (parent TournamentOptionsPanel)
     (spacing 100)
     (alignment (list 'center 'center))
     (stretchable-width #t)
     (stretchable-height #t)))
  (define AddTournamentButton
    (new
     button%
     (parent TournamentOptionsBox)
     (label "Добавить соревнование")
     (callback AddTournamentButton-callback)))
  (define EditTournamentButton
    (new
     button%
     (parent TournamentOptionsBox)
     (label "Изменить соревнование")
     (callback EditTournamentButton-callback)))
  (define DeleteTournamentButton
    (new
     button%
     (parent TournamentOptionsBox)
     (label "Удалить соревнование")
     (callback DeleteTournamentButton-callback)))
  (define TournamentBox
    (new
     group-box-panel%
     (parent TournamentTab)
     (label "Соревнования")
     (alignment (list 'center 'top))))
  (set! TournamentTable (new list-box%
                             [parent TournamentBox]
                             [choices (list )]
                             [label ""]
                             [style (list 'single 'column-headers 'variable-columns)]
                             [columns (list "Id" "Команда 1" "Команда 2" "Дата" "Счет команды 1" "Счет команды 2" "Победитель" "Проигравший")]))
  (loadTournamentData TournamentTable)
  (define RatingTab
    (new
     (class vertical-panel%
       (init parent)
       (init-field label)
       (super-new (parent parent))
       (send parent add-child-panel this label))
     (parent TabPanel)
     (label "Рейтинг")
     (alignment (list 'left 'center))
     (stretchable-width #t)
     (stretchable-height #t)))
  (define RationOptionsPanel
    (new
     group-box-panel%
     (parent RatingTab)
     (label "Возможности")
     (stretchable-width #t)
     (stretchable-height #f)))
  (define RationOptionsBox
    (new
     horizontal-pane%
     (parent RationOptionsPanel)
     (spacing 100)
     (alignment (list 'center 'center))
     (stretchable-width #t)
     (stretchable-height #t)))
  (define ReloadRatingButton
    (new
     button%
     (parent RationOptionsBox)
     (label "Обновить")
     (callback ReloadRatingButton-callback)))
  (define RatingBox
    (new
     group-box-panel%
     (parent RatingTab)
     (label "Рейтинг")
     (alignment (list 'center 'top))))
  (set! RatingTable (new list-box%
                         [parent RatingBox]
                         [choices (list )]
                         [label ""]
                         [style (list 'single 'column-headers 'variable-columns)]
                         [columns (list "Id" "Команда" "Победы" "Поражения")]))
  (loadRatingData RatingTable)
  (set! AddTeamDialog
        (new
         dialog%
         (parent MainFrame)
         (label "")
         (alignment (list 'center 'top))
         (min-width 300)
         (min-height 200)
         (stretchable-width #t)
         (stretchable-height #t)))
  (define AddTeamBox
    (new
     group-box-panel%
     (parent AddTeamDialog)
     (label "Введите данные о команде")
     (alignment (list 'center 'top))))
  (set! TeamNameField
        (new
         text-field%
         (parent AddTeamBox)
         (label "Название")
         (min-width 400)))
  (set! TeamCityField
        (new
         text-field%
         (parent AddTeamBox)
         (label "Город")
         (min-width 400)))
  (set! TeamCoachField
        (new
         text-field%
         (parent AddTeamBox)
         (label "Тренер")
         (min-width 400)))
  (define AddTeamButtonsPanel
    (new
     horizontal-pane%
     (parent AddTeamBox)
     (spacing 50)
     (alignment (list 'center 'center))))
  (define AddTeamAccept
    (new
     button%
     (parent AddTeamButtonsPanel)
     (label "Принять")
     (callback AddTeamAccept-callback)))
  (define AddTeamCancel
    (new
     button%
     (parent AddTeamButtonsPanel)
     (label "Отмена")
     (callback AddTeamCancel-callback)))
  (set! AddTournamentDialog
        (new
         dialog%
         (parent MainFrame)
         (label "Добавление матча")
         (alignment (list 'center 'top))
         (min-width 300)
         (min-height 200)
         (stretchable-width #t)
         (stretchable-height #t)))
  (define AddTournamentBox
    (new
     group-box-panel%
     (parent AddTournamentDialog)
     (label "Введите данные о матче")
     (alignment (list 'center 'top))))
  (set! ChooseFirstTeam
        (new
         choice%
         (parent AddTournamentBox)
         (label "Команда 1")
         (callback ChooseFirstTeam-callback)
         (min-width 400)
         (choices (list ))))
  (set! ChooseSecondTeam
        (new
         choice%
         (parent AddTournamentBox)
         (label "Команда 2")
         (callback ChooseSecondTeam-callback)
         (min-width 400)
         (choices (list ))))
  (set! TournamentDate
        (new
         text-field%
         (parent AddTournamentBox)
         (label "Дата")
         (min-width 400)))
  (set! Team1Score
        (new
         text-field%
         (parent AddTournamentBox)
         (label "Счет первой команды")
         (min-width 400)))
  (set! Team2Score
        (new
         text-field%
         (parent AddTournamentBox)
         (label "Счет второй команды")
         (min-width 400)))
  (define AddTournamentButtonsPanel
    (new
     horizontal-pane%
     (parent AddTournamentBox)
     (spacing 50)
     (alignment (list 'center 'center))))
  (define AddTournamentAccept
    (new
     button%
     (parent AddTournamentButtonsPanel)
     (label "Принять")
     (callback AddTournamentAccept-callback)))
  (define AddTournamentCancel
    (new
     button%
     (parent AddTournamentButtonsPanel)
     (label "Отмена")
     (callback AddTournamentCancel-callback)))

  (set! WarningDialog
        (new
         dialog%
         (parent MainFrame)
         (label "Внимание!")
         (min-width 200)
         (min-height 100)
         (stretchable-width #t)
         (stretchable-height #t)))
  (define WarningBox
    (new
     group-box-panel%
     (parent WarningDialog)
     (alignment (list 'center 'center))
     (label "")))
  (set! WarningMessage
        (new
         message%
         (parent WarningBox)
         (min-width 140)
         (label "")))
  (define WarninButtonPanel
    (new
     horizontal-pane%
     (parent WarningBox)
     (alignment (list 'center 'center))))
  (define WarningOk
    (new
     button%
     (parent WarninButtonPanel)
     (label "Ок")
     (callback WarningOk-callback)))

  (set! ConfirmDialog
        (new
         dialog%
         (parent MainFrame)
         (label "Подтвердите действие")
         (min-width 300)
         (min-height 100)
         (stretchable-width #t)
         (stretchable-height #t)))
  (define ConfirmBox
    (new
     group-box-panel%
     (parent ConfirmDialog)
     (label "")
     (alignment (list 'center 'center))))
  (set! ConfirmMessage
        (new
         message%
         (parent ConfirmBox)
         (min-width 230)
         (label "")))
  (define ConfirmButtonPanel
    (new
     horizontal-pane%
     (parent ConfirmBox)
     (alignment (list 'center 'center))
     (spacing 50)))
  (define DeleteConfirmAccept
    (new
     button%
     (parent ConfirmButtonPanel)
     (label "Принять")
     (callback DeleteConfirmAccept-callback)))
  (define DeleteConfirmCancel
    (new
     button%
     (parent ConfirmButtonPanel)
     (label "Отмена")
     (callback DeleteConfirmCancel-callback)))
  
  (send MainFrame show #t))

(module+ main (BasketballManagerGUI-init))