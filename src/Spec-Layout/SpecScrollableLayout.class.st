Class {
	#name : #SpecScrollableLayout,
	#superclass : #SpecExecutableLayout,
	#category : #'Spec-Layout-Scroll'
}

{ #category : #'instance creation' }
SpecScrollableLayout class >> with: aSymbolOrPresenter [

	^ self new 
		add: aSymbolOrPresenter;
		yourself
]

{ #category : #private }
SpecScrollableLayout >> adapterName [

	^ #ScrollableAdapter
]

{ #category : #adding }
SpecScrollableLayout >> add: aName [ 
	
	self add: aName withConstraints: [ :aConstraint | ]
]

{ #category : #private }
SpecScrollableLayout >> constraintsClass [

	^ SpecBoxConstraints
]
