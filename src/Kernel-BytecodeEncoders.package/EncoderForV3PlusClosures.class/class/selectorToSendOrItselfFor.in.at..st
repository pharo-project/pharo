instruction stream support
selectorToSendOrItselfFor: anInstructionStream in: method at: pc
	"If anInstructionStream is at a send bytecode then answer the send's selector,
	 otherwise answer anInstructionStream itself.  The rationale for answering
	 anInstructionStream instead of, say, nil, is that potentially any existing object
	 can be used as a selector, but since anInstructionStream postdates the method,
	 it can't be one of them."

	| byte byte2 |
	byte := method at: pc.
	byte < 131 ifTrue: [^ anInstructionStream].
	byte >= 176
		ifTrue: [
			"special byte or short send"
			byte >= 208
				ifTrue: [^ method literalAt: (byte bitAnd: 15) + 1]
				ifFalse: [^ Smalltalk specialSelectorAt: byte - 176 + 1]]
		ifFalse: [
			byte <= 134 
				ifTrue: [ 
					byte2 := method at: pc + 1.
					byte = 131 ifTrue: [^ method literalAt: byte2 \\ 32 + 1].
					byte = 132 ifTrue: [
						byte2 < 64 ifTrue: [^ method literalAt: ( method at: pc + 2) + 1]]	.
					byte = 133 ifTrue: [^ method literalAt: byte2 \\ 32 + 1].
					byte = 134 ifTrue: [^ method literalAt: byte2 \\ 64 + 1]]].
	^ anInstructionStream