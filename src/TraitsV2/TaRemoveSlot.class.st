"
I remove a Slot from a trait.
I implement the #--  operator.

"
Class {
	#name : #TaRemoveSlot,
	#superclass : #TaSingleComposition,
	#instVars : [
		'removedSlots'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaRemoveSlot class >> remove: anArrayOfSlotNames from: aTrait [
	^ self new
		removedSlots: anArrayOfSlotNames;
		inner: aTrait;
		yourself
]

{ #category : #copying }
TaRemoveSlot >> copyTraitExpression [
	^ self class remove: removedSlots from: inner
]

{ #category : #accessing }
TaRemoveSlot >> removedSlots [

	^ removedSlots
]

{ #category : #accessing }
TaRemoveSlot >> removedSlots: anObject [
	"we support both a symbol and an array of symbols"
	removedSlots := anObject isSymbol
		                ifTrue: [ { anObject } ]
		                ifFalse: [ anObject ]
]

{ #category : #accessing }
TaRemoveSlot >> slots [
	^ inner slots reject: [ :e | removedSlots includes: e name ]
]

{ #category : #printing }
TaRemoveSlot >> traitCompositionExpression [

	removedSlots size = 1
		           ifTrue: [^ self inner traitCompositionExpressionWithParens , ' -- ' , removedSlots first printString].

	^ String streamContents: [ :s |
		  s
			  nextPutAll: self inner traitCompositionExpressionWithParens;
			  nextPutAll: ' -- '.
		  removedSlots printAsSelfEvaluatingFormOn: s ]
]
