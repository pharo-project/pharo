initialization
swapMethod: aSelector in: aClass 
	class := self classObjectFor: aClass.
	target := self classObjectFor: (class isMeta 
			ifTrue: [ class theNonMetaClass ]
			ifFalse: [ class theMetaClass ]).
	selector := aSelector