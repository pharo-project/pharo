Class {
	#name : #SpScrollableLayout,
	#superclass : #SpExecutableLayout,
	#instVars : [
		'singleChild'
	],
	#category : #'Spec2-Layout-Scroll'
}

{ #category : #'instance creation' }
SpScrollableLayout class >> with: aSymbolOrPresenter [

	^ self new 
		add: aSymbolOrPresenter;
		yourself
]

{ #category : #private }
SpScrollableLayout >> adapterName [

	^ #ScrollableAdapter
]

{ #category : #adding }
SpScrollableLayout >> child [

	^ singleChild
]

{ #category : #adding }
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

{ #category : #adding }
SpScrollableLayout >> remove: aPresenter [

	children removeKey: aPresenter.
	singleChild := nil.
	self announceChildRemoved: aPresenter
]
