initialization
initialize
    "DelaySchedulerBackgroundWorkerMorph new openInWorld"
    | workerProcess controllerProcess |
    super initialize.
    self extent: (300@50).
    self color: Color red.
    self addMorphBack:  (stringMorph := StringMorph new).
    morphDeleted := false.      "see #delete"

    workerProcess :=  
    [     work := 0.
        [    10 milliSeconds asDelay wait.
            work := work + 1.
            morphDeleted.
        ] whileFalse.
    ] newProcess name: 'Background worker'.

    controllerProcess :=
    [     [    workerProcess resume. self color: Color green.
            10 milliSeconds asDelay wait.
            workerProcess suspend. self color: Color red.
            10 milliSeconds asDelay wait.
            morphDeleted.
        ] whileFalse.
    ] newProcess name: 'Controller'.

    controllerProcess resume.