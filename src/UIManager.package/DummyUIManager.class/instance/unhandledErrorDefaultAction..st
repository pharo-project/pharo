default actions
unhandledErrorDefaultAction: anException

	Transcript show: '*** EXCEPTION ***'; cr.
	Transcript show: anException; cr.
	Transcript show: (thisContext stack first: (20 min: thisContext stack size)); cr.
	anException isResumable ifTrue: [ anException resume ]
