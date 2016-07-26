adaptor api
morphAt: anInteger convert: aMorph

	^ morphLogic
		ifNil: [ (origin at: anInteger) asMorph ]
		ifNotNil: [
			morphLogic isSymbol
				ifTrue: [ origin perform: morphLogic withEnoughArguments: { anInteger . aMorph }  ]
				ifFalse: [ morphLogic cull: anInteger cull: aMorph ] ]