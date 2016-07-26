controlling
hasSender: context 
	"Answer whether the receiver is strictly above context on the stack."

	| senderContext |
	self == context 
		ifTrue: [^false].
	senderContext := sender.
	[senderContext == nil]
		whileFalse: [
			senderContext == context
				ifTrue: [^true].
			senderContext := senderContext sender].
	^false