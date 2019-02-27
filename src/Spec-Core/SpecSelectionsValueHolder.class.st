"
Description
--------------------

I am a value holder managing multiple selection. I should probably be integrated in NewValueHolder later. (Not for now since we are in code freeze)
"
Class {
	#name : #SpecSelectionsValueHolder,
	#superclass : #NewValueHolder,
	#instVars : [
		'indexes',
		'selections'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #accessing }
SpecSelectionsValueHolder >> indexes [
	^ indexes
]

{ #category : #accessing }
SpecSelectionsValueHolder >> indexes: anObject [
	indexes := anObject.
	self valueChanged: indexes
]

{ #category : #initialization }
SpecSelectionsValueHolder >> initialize [

	super initialize.
	indexes := OrderedCollection new asValueHolder.
	selections := OrderedCollection new asValueHolder
]

{ #category : #initialization }
SpecSelectionsValueHolder >> reset [
	self indexes value: OrderedCollection new.
	self selections value: OrderedCollection new.
	self valueChanged
]

{ #category : #accessing }
SpecSelectionsValueHolder >> selections [
	^ selections
]

{ #category : #accessing }
SpecSelectionsValueHolder >> selections: anObject [
	selections := anObject.
	self valueChanged: selections
]

{ #category : #accessing }
SpecSelectionsValueHolder >> value [
	^ {indexes . selections}
]
