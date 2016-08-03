running
checkAllPresentationsOf: exampleToTest
	| browser window |
	(Pragma
		allNamed: #gtInspectorPresentationOrder:
		from: exampleToTest class
		to: ProtoObject)
		do: [ :eachPragma | 
			browser := GLMWrapper new.
			[ browser
				show: [ :composite | 
					(eachPragma methodSelector findTokens: $:) size = 1
						ifTrue: [ exampleToTest perform: eachPragma methodSelector with: composite ].
					(eachPragma methodSelector findTokens: $:) size = 2
						ifTrue: [ exampleToTest
								perform: eachPragma methodSelector
								with: composite
								with: browser ] ].
			window := browser openOn: exampleToTest ]
				on: Error
				do: [ window ifNotNil: [ window delete ].
					self
						fail:
							eachPragma method gtDisplayString , ' for '
								, exampleToTest gtDisplayString ].
			window ifNotNil: [ window delete ] ]