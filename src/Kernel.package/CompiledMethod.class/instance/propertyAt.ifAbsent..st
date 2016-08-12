accessing-pragmas & properties
propertyAt: propName ifAbsent: aBlock
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifAbsent: aBlock]
		ifFalse: [aBlock value]