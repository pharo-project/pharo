changing suspended state
completeTo: aContext 
	"Resume self until aContext is on top"

	self suspendedContext == aContext ifTrue: [^ aContext].
	^ self complete: (self calleeOf: aContext)