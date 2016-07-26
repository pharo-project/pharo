accessing
properties
	"Answer the method properties of the receiver."
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector]
		ifFalse: [self class methodPropertiesClass forMethod: self selector: propertiesOrSelector]