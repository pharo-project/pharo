accessing
project
    project == nil
        ifTrue: [ project := self projectClass new ].
    ^ project