accessing
current
	^ Session == Smalltalk session ifTrue: [ Current ifNil: [Current := self pickDriver ]]
		ifFalse: [ 
			Session := Smalltalk session.
			Current := self pickDriver
	]
	