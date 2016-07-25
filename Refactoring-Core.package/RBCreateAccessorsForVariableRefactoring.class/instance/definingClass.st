private-accessing
definingClass
	^ classVariable 
		ifTrue: [ class theMetaClass ]
		ifFalse: [ class ]