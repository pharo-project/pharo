private-exceptions
findNextHandlerContext
	"Return the next handler marked context, returning nil if there is none.  Search starts with self and proceeds up to nil."

	| context |
	context := self findNextHandlerOrSignalingContext.
	context isNil
		ifTrue: [ ^ nil ].
	context isHandlerContext
		ifTrue: [ ^ context ].	"If it isn't a handler context, it must be a signaling context.
	When we reach a signaling context we must skip over any handlers
	that might be on the stack between the signaling context and the handler
	context for that signal."
	^ context exception privHandlerContext nextHandlerContext