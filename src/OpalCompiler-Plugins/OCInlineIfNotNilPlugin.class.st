Class {
	#name : #OCInlineIfNotNilPlugin,
	#superclass : #OCInlineConditionalsWithoutParametersPlugin,
	#category : #'OpalCompiler-Plugins'
}

{ #category : #'class initialization' }
OCInlineIfNotNilPlugin class >> initialize [

	self install
]

{ #category : #api }
OCInlineIfNotNilPlugin >> newBlockStatement: toBeBlock argument: blockArgument [

	^ (toBeBlock isLiteralNode and: [toBeBlock value isSymbol not])
		  ifTrue: [ toBeBlock copy ]
		  ifFalse: [ 
			  RBMessageNode
				  receiver: toBeBlock copy
				  selector: #cull:
				  arguments: { blockArgument } ]
]

{ #category : #api }
OCInlineIfNotNilPlugin >> replaceNodeWithBlock: toBeBlock [

	| newNode blockStatement blockArgument argumentIndex |

	argumentIndex := toBeBlock parent arguments indexOf: toBeBlock.
	(toBeBlock parent selector keywords at: argumentIndex) = 'ifNotNil:' 
		ifFalse: [ ^ super replaceNodeWithBlock: toBeBlock ].

	"I need to generate an unused variable name"
	blockArgument := RBVariableNode named: Object new hash asString asSymbol.
	blockStatement := self newBlockStatement: toBeBlock argument: blockArgument.

	newNode := RBBlockNode arguments: {blockArgument} body: (RBSequenceNode statements: { blockStatement }).
	toBeBlock replaceWith: newNode

]

{ #category : #api }
OCInlineIfNotNilPlugin >> whiteList [
	
	^ #(ifNotNil: ifNil:ifNotNil: ifNotNil:ifNil:)
]
