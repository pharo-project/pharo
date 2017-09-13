running
check: aMethod forCritiquesDo: aCriticBlock
	| definer superMethod |
	aMethod methodClass isMeta ifTrue: [ ^ self ].
	(self methodsRequiringSuper includes: aMethod selector) ifFalse: [ ^ self ]. 
	
	definer := aMethod methodClass superclass ifNotNil: [ :sc |
		sc whichClassIncludesSelector: aMethod selector ].
	definer ifNil: [ ^ self ].
	
	"super defines same method"
	(aMethod superMessages includes: aMethod selector) ifTrue: [ ^self ]. 
	
	"but I don't call it"
	superMethod := definer 
		compiledMethodAt: aMethod selector
		ifAbsent: [  ].
	
	superMethod isReturnSelf ifTrue: [ ^ self ].
	(superMethod sendsSelector: #subclassResponsibility) ifTrue: [ ^ self ].
	
	aCriticBlock cull: (self critiqueFor: aMethod)