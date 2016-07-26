initialization
initialize
	super initialize.
	self matcher 
		matchesAnyOf: #(
			'^`@condition ifTrue: [| `@temps | `@.statements]'
			'^`@condition ifFalse: [| `@temps | `@.statements]' )
		do: [ :node :answer | node ]