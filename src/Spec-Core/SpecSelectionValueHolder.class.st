"
Description
--------------------

I am a copied class from NewValueHolder (SelectionValueHolder) to correct a bug. Later wy code should be moved to Pharo directly, but for now we are in code freeze and I need to make it work.
"
Class {
	#name : #SpecSelectionValueHolder,
	#superclass : #NewValueHolder,
	#instVars : [
		'index',
		'selection'
	],
	#category : #'Spec-Core-Support'
}

{ #category : #'instance creation' }
SpecSelectionValueHolder class >> index: indexValueHolder selection: selectionValueHolder [

	^ self new
		index: indexValueHolder;
		selection: selectionValueHolder;
		yourself
]

{ #category : #accessing }
SpecSelectionValueHolder >> index [

	^ index
]

{ #category : #accessing }
SpecSelectionValueHolder >> index: anIndex [

	index := anIndex.
	self valueChanged: index
]

{ #category : #initialization }
SpecSelectionValueHolder >> initialize [

	super initialize.
	index := 0 asValueHolder.
	selection := nil asValueHolder.
	
	index whenChangedSend: #valueChanged to: self.
	selection whenChangedSend: #valueChanged to: self.
]

{ #category : #protocol }
SpecSelectionValueHolder >> reset [

	self index value: 0.
	self selection value: nil.
	self valueChanged
]

{ #category : #accessing }
SpecSelectionValueHolder >> selection [

	^ selection
]

{ #category : #accessing }
SpecSelectionValueHolder >> selection: aSelection [

	selection := aSelection.
	self valueChanged: selection
]

{ #category : #accessing }
SpecSelectionValueHolder >> value [

	^ {index. selection}
]
