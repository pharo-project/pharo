private
checkAllPresentationsOf: exampleToTest onError: aBlock
	| browser window |
	(Pragma allNamed: #gtInspectorPresentationOrder: from: exampleToTest class to: ProtoObject) do: [ :eachPragma | 
		browser := GLMWrapper new.
		[	browser
				show: [ :composite | 
					(eachPragma selector findTokens: $:) size = 1
						ifTrue: [ exampleToTest perform: eachPragma selector with: composite ].
					(eachPragma selector findTokens: $:) size = 2
						ifTrue: [ exampleToTest perform: eachPragma selector with: composite with: browser ] ].
			window := browser openOn: exampleToTest ] 
				on: Error 
				do: [ :exception |
					window ifNotNil: [ window delete ].
					aBlock value: eachPragma value: exception ].
		window ifNotNil: [ window delete ] ]