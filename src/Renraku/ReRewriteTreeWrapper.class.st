"
A concrete wrapper implementation that uses a rewrite expression to create a new node
"
Class {
	#name : #ReRewriteTreeWrapper,
	#superclass : #ReRewriteWrapper,
	#instVars : [
		'tree'
	],
	#category : #Renraku-Rules-Rewrite
}

{ #category : #'instance creation' }
ReRewriteTreeWrapper class >> on: aTree [

	^ self basicNew
		initializeTree: aTree;
		yourself
]

{ #category : #initialization }
ReRewriteTreeWrapper >> initializeTree: aTree [
	tree := aTree
]

{ #category : #actions }
ReRewriteTreeWrapper >> rewriten: aNode with: aMap [

	^ tree copyInContext: aMap
]
