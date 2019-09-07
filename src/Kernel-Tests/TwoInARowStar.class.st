"
I'm an helper class that implements the `two-in-a-row*` function for testing continuations.
"
Class {
	#name : #TwoInARowStar,
	#superclass : #Object,
	#instVars : [
		'leave',
		'fill'
	],
	#category : #'Kernel-Tests-Methods'
}

{ #category : #private }
TwoInARowStar >> T: anObj [
	| n |
	n := self getNext.
	^ n isSymbol
		ifTrue: [ n = anObj or: [ self T: n ] ]
		ifFalse: [ false ]
]

{ #category : #private }
TwoInARowStar >> getNext [

	^ Continuation currentDo: [ :here_again | leave := here_again. fill value: #go ]
]

{ #category : #API }
TwoInARowStar >> tree: aTree [
	| fst |
	fst := Continuation
		currentDo: [ :here | 
			leave := here.
			self waddle: aTree.
			leave value: nil ].
	^ fst isSymbol
		ifTrue: [ self T: fst ]
		ifFalse: [ false ]
]

{ #category : #private }
TwoInARowStar >> waddle: aTree [
	^ (aTree isNotNil and: [ (aTree isMemberOf: Link) not ])
		ifTrue: [ | car cdr |
			car := aTree value.
			cdr := aTree nextLink.
(			car isKindOf:  Link)
				ifTrue: [ self waddle: car ]
				ifFalse: [ Continuation
						currentDo: [ :rest | 
							fill := rest.
							leave value: car ] ].
			self waddle: cdr ]
]
