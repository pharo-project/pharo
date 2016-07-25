accessing context
currentContext
	"I answer the context on which an action should be executed.
	This is either the selected context or if nothing is selected the interrupted context."
	
	^ self selectedContext isNil 
		ifTrue: [ self interruptedContext ]
		ifFalse: [ self selectedContext ]