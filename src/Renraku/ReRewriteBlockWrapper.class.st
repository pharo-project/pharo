"
A concrete wrapper implementation that uses a block to create new nodes
"
Class {
	#name : #ReRewriteBlockWrapper,
	#superclass : #ReRewriteWrapper,
	#instVars : [
		'block'
	],
	#category : #Renraku-Rules-Rewrite
}

{ #category : #'instance creation' }
ReRewriteBlockWrapper class >> on: aBlock [

	^ self basicNew
		initializeBlock: aBlock;
		yourself
]

{ #category : #initialization }
ReRewriteBlockWrapper >> initializeBlock: aBlock [
	block := aBlock
]

{ #category : #actions }
ReRewriteBlockWrapper >> rewriten: aNode with: aMap [

	^ block cull: aNode cull: aMap
]
