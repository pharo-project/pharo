"
I represent a class modification. 

I content field modifications.
"
Class {
	#name : #AbstractClassModification,
	#superclass : #Object,
	#instVars : [
		'target',
		'layout',
		'propagations',
		'newClass'
	],
	#category : #'Slot-ClassBuilder'
}

{ #category : #accessing }
AbstractClassModification >> allPropagationsDo: aBlock [
	propagations do: [ :aPropagation |
		aBlock value: aPropagation.
		aPropagation allPropagationsDo: aBlock ]
]

{ #category : #public }
AbstractClassModification >> apply [
	^ newClass := target shallowCopy
		superclass: self superclass
		layout: layout
]

{ #category : #accessing }
AbstractClassModification >> checkSanity [
	layout checkSanity.
]

{ #category : #accessing }
AbstractClassModification >> classLayout [
	^ self layout
]

{ #category : #accessing }
AbstractClassModification >> classLayout: aLayout [
	self layout: aLayout
]

{ #category : #private }
AbstractClassModification >> computeChange [
	self checkSanity.
]

{ #category : #initialization }
AbstractClassModification >> initialize [
	propagations := OrderedCollection new.
]

{ #category : #testing }
AbstractClassModification >> isPropagation [
	^ self subclassResponsibility 
]

{ #category : #accessing }
AbstractClassModification >> layout [
	^ layout
]

{ #category : #accessing }
AbstractClassModification >> layout: aLayout [
	layout := aLayout.
	layout host: target.
	layout finalize.
	layout = target classLayout
		ifFalse: [ self computeChange ]
]

{ #category : #accessing }
AbstractClassModification >> newClass [
	^ newClass
]

{ #category : #accessing }
AbstractClassModification >> newLayout [
	^ layout
]

{ #category : #accessing }
AbstractClassModification >> oldLayout [
	^ target classLayout
]

{ #category : #private }
AbstractClassModification >> propagate [
	target subclassesDo: [ :subclass |
		propagations add:
			(ClassModificationPropagation
				propagate: self
				to: subclass) ].
	"recursively propagate this change down"
	propagations do: [ :propagation | propagation propagate ]
	
		
]

{ #category : #accessing }
AbstractClassModification >> result [
	^ target
]

{ #category : #accessing }
AbstractClassModification >> target [
	^ target
]

{ #category : #accessing }
AbstractClassModification >> target: aTarget [
	target := aTarget
]
