accessing
activeHome
	"If executing closure, search senders for the activation of the original
	 (outermost) method that (indirectly) created my closure (the closureHome).
	 If the closureHome is not found on the sender chain answer nil."

	| methodReturnContext |
	self isBlockContext ifFalse: [^self].
	self sender ifNil: [^nil].
	methodReturnContext := self methodReturnContext.
	^self sender findContextSuchThat: [:ctxt | ctxt = methodReturnContext]