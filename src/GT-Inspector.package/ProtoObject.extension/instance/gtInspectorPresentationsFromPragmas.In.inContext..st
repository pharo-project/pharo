*GT-Inspector
gtInspectorPresentationsFromPragmas: aCollection In: composite inContext: aGTInspector

	aCollection
		do: [ :eachPragma | 
			eachPragma selector numArgs = 0
					ifTrue: [ 
						| presentationSource |
						presentationSource := self perform: eachPragma selector.
						presentationSource glmPresentation cull: composite cull: aGTInspector cull: self ].	
			eachPragma selector numArgs = 1
				ifTrue: [ 
					thisContext 
						object: self 
						perform: eachPragma selector 
						withArguments: { composite } 
						inClass: self class ].
			eachPragma selector numArgs = 2
				ifTrue: [ 
					thisContext 
						object: self 
						perform: eachPragma selector 
						withArguments: { composite . aGTInspector } 
						inClass: self class ] ]