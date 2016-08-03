*GT-Inspector
gtInspectorPresentationsFromPragmas: aCollection In: composite inContext: aGTInspector
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