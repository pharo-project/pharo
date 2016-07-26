accessing-pragmas & properties
pragmaAt: aKey
	"Answer the pragma with selector aKey, or nil if none."
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector at: aKey ifAbsent: [nil]]
		ifFalse: [nil]