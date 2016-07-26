binding
bindingOf: aString 
	^ interactionModel ifNotNil:  [ interactionModel bindingOf: aString  ]