accessing-pragmas & properties
propertyKeysAndValuesDo: aBlock
	"Enumerate the receiver with all the keys and values."

	| propertiesOrSelector |
	(propertiesOrSelector := self penultimateLiteral) isMethodProperties ifTrue:
		[propertiesOrSelector propertyKeysAndValuesDo: aBlock]