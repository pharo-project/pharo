system simulation
stepToHome: aContext 
	"Resume self until the home of top context is aContext.  Top context may be a block context."
	| home activeContext here error homeSender |
	homeSender := aContext sender.
	here := thisContext. 	
	"Insert ensure and exception handler contexts under aSender"
	aContext insertSender: (Context
		contextOn: Exception do: [:ex |
			here push: { thisContext sender. ex }. "it will return special value from ctxt step message"
			here jump]).
	home := aContext home.
	activeContext := self.	
	[activeContext := activeContext step.
	activeContext isArray ifTrue: [ 
		error := activeContext last. 
		activeContext := activeContext first ].
	home isDead | error notNil | (home == activeContext home)] whileFalse.

	aContext terminateTo: homeSender.
	^ {activeContext. error}