Class {
	#name : #SpVerticalAlignment,
	#superclass : #SpAbstractPresenter,
	#traits : 'SpTObservable',
	#classTraits : 'SpTObservable classTrait',
	#instVars : [
		'#firstVisibleRowIndex',
		'#lastVisibleRowIndex',
		'#desiredVisibleRow => SpObservableSlot'
	],
	#category : #'Spec2-Core-Widgets-Table'
}

{ #category : #accessing }
SpVerticalAlignment >> desiredVisibleRow [
	^ desiredVisibleRow
]

{ #category : #accessing }
SpVerticalAlignment >> desiredVisibleRow: anInteger [
	desiredVisibleRow := anInteger
]

{ #category : #accessing }
SpVerticalAlignment >> firstVisibleRowIndex [
	"valid only after UI is open"

	^ firstVisibleRowIndex
]

{ #category : #accessing }
SpVerticalAlignment >> firstVisibleRowIndex: anInteger [
	firstVisibleRowIndex := anInteger
]

{ #category : #initialization }
SpVerticalAlignment >> initialize [
	self class initializeSlots: self.
	super initialize.
	desiredVisibleRow := 1
]

{ #category : #initialization }
SpVerticalAlignment >> lastVisibleRowIndex [
	^ lastVisibleRowIndex
]

{ #category : #initialization }
SpVerticalAlignment >> lastVisibleRowIndex: anInteger [
	lastVisibleRowIndex := anInteger
]

{ #category : #initialization }
SpVerticalAlignment >> whenChangedDo: aBlockClosure [
	self property: #desiredVisibleRow whenChangedDo: aBlockClosure
]
