widget API
aboutTitle
    ^ self model model
         ifNotNil: [ :m | (m respondsTo: #aboutTitle)
		ifTrue: [m aboutTitle]
		ifFalse: [m class name]].