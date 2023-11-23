"
I am an adaptor between the Morphic text morphs supporting completion and Complishon.
I contain a complishonBuilder that is used to create a complishon.

See my superclass for details
"
Class {
	#name : 'CoCompletionEngine',
	#superclass : 'CompletionEngine',
	#instVars : [
		'completionBuilder'
	],
	#category : 'HeuristicCompletion-Morphic-Core',
	#package : 'HeuristicCompletion-Morphic',
	#tag : 'Core'
}

{ #category : 'accessing' }
CoCompletionEngine >> completionBuilder [

	^ completionBuilder ifNil: [ CoASTHeuristicsResultSetBuilder new ]
]

{ #category : 'accessing' }
CoCompletionEngine >> completionBuilder: anObject [
	completionBuilder := anObject
]

{ #category : 'menu morph' }
CoCompletionEngine >> createContext [

	^ super createContext
		completionBuilder: self completionBuilder;
		yourself
]

{ #category : 'accessing' }
CoCompletionEngine >> doItContext [
	"Rubric sends the morph as requestor to the compiler.
	We need to use it to keep the same semantics..."

	^ self editor model doItContext
]

{ #category : 'accessing' }
CoCompletionEngine >> doItRequestor [

	^ self editor morph
]

{ #category : 'initialization' }
CoCompletionEngine >> initialize [

	super initialize.
	contextClass := CoCompletionContext
]

{ #category : 'accessing' }
CoCompletionEngine >> variableBindingNames [

	^ self doItRequestor
		ifNil: [ #() ]
		ifNotNil: [ :requestor | requestor model variableBindings keys ]
]
