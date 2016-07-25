accessing-pragmas & properties
propertyValueAt: propName ifAbsent: aBlock
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyValueAt: propName ifAbsent: aBlock]
		ifFalse: [aBlock value]