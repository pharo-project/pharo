Extension { #name : #TraitBehavior }

{ #category : #'*Ring2-Core' }
TraitBehavior >> ensureRing2DefinitionIn: aRG2Environment [

	^ aRG2Environment ask behaviors 
		detect: [:each | each name = self name] 
		ifNone: [ 
			| behaviorModel |
			behaviorModel := self asRing2MinimalDefinitionIn: aRG2Environment.
			aRG2Environment addBehavior: behaviorModel.
			behaviorModel]
]
