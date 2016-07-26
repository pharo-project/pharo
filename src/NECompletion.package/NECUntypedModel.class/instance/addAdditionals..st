private
addAdditionals: aCollection 
	aCollection
		ifNotNil: [aCollection
				do: [:each | each isLocal
						ifTrue: [localVars add: each value]
						ifFalse: [each isInstance
								ifTrue: [instVars add: each value]
								ifFalse: [each isSelector
										ifTrue: [selectors add: each value]]]]]