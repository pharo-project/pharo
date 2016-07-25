access
methodClassName
	^ meta
		ifTrue: [ class , ' class' ]
		ifFalse: [ class ]