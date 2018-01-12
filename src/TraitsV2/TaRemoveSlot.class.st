Class {
	#name : #TaRemoveSlot,
	#superclass : #TaSingleComposition,
	#instVars : [
		'slotName'
	],
	#category : #'TraitsV2-Compositions'
}

{ #category : #'instance creation' }
TaRemoveSlot class >> named: anSlotName from: aTrait [
	^ self new
		slotName: anSlotName;
		inner: aTrait;
		yourself.
]

{ #category : #copying }
TaRemoveSlot >> copyTraitExpression [
	^ self class named: slotName from: inner.
]

{ #category : #accessing }
TaRemoveSlot >> slotName [
	^ slotName
]

{ #category : #accessing }
TaRemoveSlot >> slotName: anObject [
	slotName := anObject
]

{ #category : #accessing }
TaRemoveSlot >> slots [
	^ inner slots reject: [ :e | e name = slotName ]
]

{ #category : #printing }
TaRemoveSlot >> traitCompositionExpression [

	^ (self inner traitCompositionExpressionWithParens , ' -- ' , slotName printString) asString
]
