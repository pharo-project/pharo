Extension { #name : #CompletionEngine }

{ #category : #'*HeuristicCompletion' }
CompletionEngine >> doItContext [
	"Rubric sends the morph as requestor to the compiler.
	We need to use it to keep the same semantics..."

	^ self editor model doItContext
]

{ #category : #'*HeuristicCompletion' }
CompletionEngine >> doItRequestor [

	^ self editor morph
]

{ #category : #'*HeuristicCompletion' }
CompletionEngine >> variableBindingNames [

	self doItRequestor ifNil: [ ^ #() ].
	^ self doItRequestor model variableBindings keys
]
