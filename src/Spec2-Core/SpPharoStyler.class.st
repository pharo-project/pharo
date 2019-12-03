Class {
	#name : #SpPharoStyler,
	#superclass : #SpSourceStyler,
	#instVars : [
		'behavior'
	],
	#category : #'Spec2-Core-Widgets-Text'
}

{ #category : #applying }
SpPharoStyler >> applyStyleTo: aString [

	^ self withAdapterDo: [ :anAdapter |
		anAdapter applyStyleTo: aString ]
]

{ #category : #parsing }
SpPharoStyler >> parseAST: aString [
	| ast |

	ast := RBParser parseFaultyMethod: aString.
	ast isParseError ifFalse: [ ^ ast ].

	^ RBParser parseFaultyExpression: aString	
]

{ #category : #'as yet unclassified' }
SpPharoStyler >> prepareWithinContext: anObject [

	behavior := anObject behavior
]
