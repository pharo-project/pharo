default actions
syntaxErrorNotificationDefaultAction: anException

	Transcript show: '*** SYNTAX ERROR ***'; cr.
	Transcript show: anException; cr.
	Transcript show: (thisContext stack first: (20 min: thisContext stack size)); cr.
	anException return.