changing suspended state
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context."

	| home ctxt |
	home := aContext home.
	[	ctxt := self step.
		home == ctxt home.
	] whileFalse: [
		home isDead ifTrue: [^ self suspendedContext].
	].
	^ self suspendedContext