code generation
selectorNode
	"Answer a SelectorNode for the message selector of the method represented by the receiver."

	^(selectorOrFalse isMemberOf: SelectorNode)
		ifTrue: [selectorOrFalse]
		ifFalse: [SelectorNode new key: selectorOrFalse]