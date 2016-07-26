accessing-pragmas & properties
propertyValueAt: propName
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyValueAt: propName ifAbsent: [nil]]
		ifFalse: [nil]