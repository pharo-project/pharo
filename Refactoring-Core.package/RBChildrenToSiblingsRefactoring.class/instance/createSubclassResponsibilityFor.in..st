private-methods
createSubclassResponsibilityFor: aSelector in: aClass 
	| source |
	(aClass superclass definesMethod: aSelector) ifTrue: [^self].
	source := self subclassResponsibilityFor: aSelector in: aClass.
	source isNil ifTrue: [^self].
	aClass superclass compile: source
		classified: (aClass protocolsFor: aSelector)