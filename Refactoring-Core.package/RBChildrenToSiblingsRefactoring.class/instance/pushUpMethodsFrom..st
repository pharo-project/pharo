private-methods
pushUpMethodsFrom: aClass 
	| selectorsToPushUp |
	selectorsToPushUp := self selectorsToPushUpFrom: aClass.
	aClass selectors do: 
			[:each | 
			(selectorsToPushUp includes: each) 
				ifTrue: [self pushUp: each in: aClass]
				ifFalse: [self createSubclassResponsibilityFor: each in: aClass]].
	selectorsToPushUp do: [:each | aClass removeMethod: each]