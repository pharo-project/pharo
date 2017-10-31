initialization
initialize
	super initialize.
	self matchesAny: #(
			'^`@condition ifTrue: [| `@temps | `@.statements]'
			'^`@condition ifFalse: [| `@temps | `@.statements]' )