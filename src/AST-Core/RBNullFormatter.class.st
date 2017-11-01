"
A simple null formatter that just delegates  to source code and is not good when nodes are reflectively built.

Will be replaced by RBSimpleFormatter
"
Class {
	#name : #RBNullFormatter,
	#superclass : #RBProgramNodeVisitor,
	#instVars : [
		'codeStream'
	],
	#classVars : [
		'FormatAsYouReadPolicy'
	],
	#category : 'AST-Core-Formatter'
}

{ #category : #initialize }
RBNullFormatter class >> formatAsYouReadPolicy [
	"Return whether the formatter is continously formatting"

	^ FormatAsYouReadPolicy ifNil: [ FormatAsYouReadPolicy := false ]

]

{ #category : #initialize }
RBNullFormatter class >> formatAsYouReadPolicy: aBoolean [
	"Return whether the formatter is continously formatting"

	FormatAsYouReadPolicy := aBoolean
]

{ #category : #'public interface' }
RBNullFormatter >> containerWidth: anInteger [
	"doing nothing on purpose"
]

{ #category : #'public interface' }
RBNullFormatter >> format: aParseTree [ 
	^ aParseTree source
		ifNil: [	self visitNode: aParseTree.
					codeStream contents ]
]

{ #category : #initialization }
RBNullFormatter >> initialize [
	super initialize.
	codeStream := (String new: 256) writeStream.
]

{ #category : #visiting }
RBNullFormatter >> visitMethodNode: aMethodNode [
	codeStream 
		nextPutAll: aMethodNode selector;
		cr;
		nextPutAll: '	''no source and no formatter available!'''
]
