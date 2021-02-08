Class {
	#name : #SpScrollableLayout,
	#superclass : #SpExecutableLayout,
	#traits : 'SpTAlignable',
	#classTraits : 'SpTAlignable classTrait',
	#instVars : [
		'singleChild'
	],
	#category : #'Spec2-Layout-Scroll'
}

{ #category : #'instance creation' }
SpScrollableLayout class >> with: aSymbolOrPresenter [

	^ self new 
		child: aSymbolOrPresenter;
		yourself
]

{ #category : #private }
SpScrollableLayout >> adapterName [

	^ #ScrollableAdapter
]

{ #category : #accessing }
SpScrollableLayout >> child [

	^ singleChild
]

{ #category : #accessing }
SpScrollableLayout >> child: aPresenter [

	singleChild ifNotNil: [ self remove: singleChild ].
	self
		add: aPresenter
		withConstraints: [ :constraints | ].
	singleChild := aPresenter.
	self announceChildAdded: aPresenter
]

{ #category : #private }
SpScrollableLayout >> constraintsClass [

	^ SpBoxConstraints
]

{ #category : #accessing }
SpScrollableLayout >> remove: aPresenter [

	singleChild := nil.
	super remove: aPresenter
	
]

{ #category : #accessing }
SpScrollableLayout >> scrollTo: aPoint [

	self withAdapterDo: [ :anAdapter |
		anAdapter scrollTo: aPoint ]
]
