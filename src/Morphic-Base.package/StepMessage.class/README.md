I'm a MorphicAlarm to be queued in WorldState list of stepping actions. I hold the stepTime (refresh period) of my receiver / Morph (if nil, the #stepTime value of my receiver is used).

See 
- WordState>>runLocalStepMethodsIn:   (where the stepping actions are sent).
- WorldState>>startStepping:at:selector:arguments:stepTime:  (to queue a new StepMessage)

Example:

MorphicUIManager currentWorld
        startStepping: (Morph new openInWorld)
        at: Time millisecondClockValue
        selector: #toggleVisible
        arguments: nil
        stepTime: 500.