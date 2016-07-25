private-exceptions
findNextHandlerOrSignalingContext
	"Return the next handler/signaling marked context, answering nil if there is none. 
	Search starts with self and proceeds up to nil."

	<primitive: 197>
	| context |
	context := self.
	[ 
	context isHandlerOrSignalingContext
		ifTrue: [ ^ context ].
	(context := context sender) == nil ] whileFalse.
	^ nil