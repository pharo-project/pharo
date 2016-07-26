changing suspended state
completeStep: aContext 
	"Resume self until aContext is on top, or if already on top, complete next step"

	| callee |
	self suspendedContext == aContext ifFalse: [
		^ self complete: (self calleeOf: aContext)].
	callee := self step.
	callee == aContext ifTrue: [^ callee].
	aContext isDead ifTrue: [^ self suspendedContext].  "returned"
	^ self complete: callee  "finish send"