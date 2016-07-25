debugging actions
peelToFirstLike: aContext
	"Peel the stack back to the second occurance of the currently selected message.  Very useful for an infinite recursion.  Gets back to the second call so you can see one complete recursion cycle, and how it was called at the beginning.  Also frees a lot of space!"

	| ctxt |
	self flag: 'should be called only on the selected context'.	
	(self isContextPostMortem: aContext) ifTrue: [^ self].

	ctxt := interruptedProcess popTo: aContext findSecondToOldestSimilarSender.
	self updateContextTo: ctxt.
	self contextChanged 

	