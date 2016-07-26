accessing values
titleIconValue
	^ self titleIcon notNil
		ifTrue: [ self titleIcon glamourValue: self entity ]
		ifFalse: [ self titleIcon ]