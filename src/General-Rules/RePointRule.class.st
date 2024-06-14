"
This smell arise when a method contains a creation of an instance Point in this way : 

Point x: y:

prefer use x@y to create an instance of Point.
"
Class {
	#name : 'RePointRule',
	#superclass : 'ReNodeBasedRule',
	#category : 'General-Rules-Optimization',
	#package : 'General-Rules',
	#tag : 'Optimization'
}

{ #category : 'accessing' }
RePointRule class >> group [

	^ 'Optimization'
]

{ #category : 'accessing' }
RePointRule class >> rationale [ 

	^ 'it''s much faster to use x@y than Point x:x y:y' 
]

{ #category : 'accessing' }
RePointRule class >> ruleName [
	^ 'prefer to use x@y instead of Point x: x y: y'
]

{ #category : 'manifest' }
RePointRule class >> uniqueIdentifierName [
	"This number should be unique and should change only when the rule completely change semantics"

	^ 'PointRule'
]

{ #category : 'running' }
RePointRule >> basicCheck: aNode [

	aNode isMessage ifFalse: [ ^ false ].
	aNode receiver isMessage ifTrue: [ ^ false ].
	aNode isSelfSend ifTrue: [ ^ false ].
	^ (#( #x:y: ) includes: aNode selector) 
		and: [ aNode receiver name == Point name  ]
]
