"
I sort methods in the way that binary methods will be first in the ascending list.
"
Class {
	#name : #ClySortMethodBySelectorFunction,
	#superclass : #ClySortFunction,
	#category : #'Calypso-SystemQueries-Queries'
}

{ #category : #evaluating }
ClySortMethodBySelectorFunction >> collate: aMethod1 with: aMethod2 [

	(aMethod1 selector isBinary and: [aMethod2 selector isBinary not]) ifTrue: [ ^-1 ].
	(aMethod1 selector isBinary not and: [aMethod2 selector isBinary]) ifTrue: [ ^1 ].
	
	aMethod1 selector = aMethod2 selector ifTrue: [ 
		^aMethod1 origin name threeWayCompareTo: aMethod2 origin name ].
	
	^aMethod1 selector threeWayCompareTo: aMethod2 selector
]
