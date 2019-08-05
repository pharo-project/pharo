"
I am especially designed to contain a list selection (an index + an object)
"
Class {
	#name : #SelectionValueHolder,
	#superclass : #NewValueHolder,
	#instVars : [
		'index',
		'selection'
	],
	#category : #'NewValueHolder-Core-Base'
}

{ #category : #'instance creation' }
SelectionValueHolder class >> index: indexValueHolder selection: selectionValueHolder [

	^ self new
		index: indexValueHolder;
		selection: selectionValueHolder;
		yourself
]

{ #category : #accessing }
SelectionValueHolder >> index [

	^ index
]

{ #category : #accessing }
SelectionValueHolder >> index: anIndex [

	index := anIndex.
	self valueChanged: index
]

{ #category : #initialization }
SelectionValueHolder >> initialize [

	super initialize.
	index := 0 asValueHolder.
	selection := nil asValueHolder.
	
	index whenChangedSend: #valueChanged to: self.
	selection whenChangedSend: #valueChanged to: self.
]

{ #category : #initialization }
SelectionValueHolder >> reset [

	self index value: 0.
	self selection value: nil.
	self valueChanged
]

{ #category : #accessing }
SelectionValueHolder >> selection [

	^ selection
]

{ #category : #accessing }
SelectionValueHolder >> selection: aSelection [

	selection := aSelection.
	self valueChanged: selection
]

{ #category : #accessing }
SelectionValueHolder >> value [

	^ {index. selection}
]
