Extension { #name : 'RubScrolledTextModel' }

{ #category : '*HeuristicCompletion-Morphic' }
RubScrolledTextModel >> variableBindings [
	"Required for polymorphism with the Playground"
	^ interactionModel
		ifNil: [ #() asDictionary ]
		ifNotNil: [ interactionModel bindings ]
]
