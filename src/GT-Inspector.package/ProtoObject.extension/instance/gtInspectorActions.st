*GT-Inspector
gtInspectorActions
	"This is a utility method that allows each object to define a set of actions 
	specific for itself. These actions are used when the object appears in the inspector.

	The actions are collected based on the #gtInspectorAction pragma that are defined in
	unary methods corresponding to the body of the actions"

	| all |
	all := (Pragma
		allNamed: #gtInspectorAction
		from: self class
		to: ProtoObject)
		collect: [ :eachPragma | 
			thisContext
				object: self
				perform: eachPragma methodSelector
				withArguments: #()
				inClass: self class ].
	^ all
		asSortedCollection: [ :a :b | 
			| first second |
			first := a isCategorized
				ifTrue: [ a category , a title ]
				ifFalse: [ a title ].
			second := b isCategorized
				ifTrue: [ b category , b title ]
				ifFalse: [ b title ].
			first < second ]