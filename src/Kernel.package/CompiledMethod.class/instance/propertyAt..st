accessing-pragmas & properties
propertyAt: propName
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifAbsent: [nil]]
		ifFalse: [nil]