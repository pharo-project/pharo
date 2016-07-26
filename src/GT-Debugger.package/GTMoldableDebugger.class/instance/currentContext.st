accessing context
currentContext

	^self selectedContext 
		ifNil: [ self interruptedContext ] 
		ifNotNil:  [ self selectedContext  ]