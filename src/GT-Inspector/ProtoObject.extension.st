Extension { #name : #ProtoObject }

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorActionBrowse [
	<gtInspectorAction>
	^ GLMGenericAction new
		action: [ Smalltalk tools browser openOnClass: self class ];
		icon: GLMUIThemeExtraIcons glamorousBrowse;
		title: 'Browse'
]

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorActions [
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
]

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorIcon [
	^ nil
]

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorMetaIn: composite [
	"This presentation offers the methods of the class of the current object"
	<gtInspectorPresentationOrder: 1000>
	<gtInspectorTag: #basic>
	^ composite custom: (GTSimpleMethodsBrowser new startOn: self) 
]

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorPresentationsFromPragmas: aCollection In: composite inContext: aGTInspector [
	aCollection
		do: [ :eachPragma | 
			eachPragma methodSelector numArgs = 0
				ifTrue: [ | presentationSource |
					presentationSource := self perform: eachPragma selector.
					presentationSource glmPresentation
						cull: composite
						cull: aGTInspector
						cull: self ].
			eachPragma methodSelector numArgs = 1
				ifTrue: [ thisContext
						object: self
						perform: eachPragma methodSelector
						withArguments: {composite}
						inClass: self class ].
			eachPragma methodSelector numArgs = 2
				ifTrue: [ thisContext
						object: self
						perform: eachPragma methodSelector
						withArguments:
							{composite.
							aGTInspector}
						inClass: self class ] ]
]

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorPresentationsIn: composite inContext: aGTInspector [
	"This is a utility method that collects all presentations for the current object.
	By default, it simply looks for the #gtInspectorPresentationOrder: pragma.
	The inspector can decice whether or not a presentation should be dispayed.
	Subclasses might want to override it for more special behavior."
	| pragmas |
	pragmas := Pragma
		allNamed: aGTInspector class extensionsPragma
		from: self class
		to: ProtoObject
		sortedUsing: [ :x :y | (x argumentAt: 1) < (y argumentAt: 1) ].
	pragmas := pragmas select: [ :aPragma | aGTInspector shouldDisplayPresentationCreatedBy: aPragma method ].
	self gtInspectorPresentationsFromPragmas: pragmas In: composite inContext: aGTInspector
]

{ #category : #'*GT-Inspector' }
ProtoObject >> gtInspectorRawIn: composite [
	"This is the most basic presentation showing the state of the object"
	<gtInspectorPresentationOrder: 10>
	<gtInspectorTag: #basic>
	^ composite custom: GTProtoObjectVariablesBrowser new
]
