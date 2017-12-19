"
AbstractWidget is an abstraction for the different widget which could be used to compose the Nautilus UI.

Note however that the design is ugly because it mixes UI and Model. 
All the code in this hierarchy is conceptually bogus and should be rewritten.
"
Class {
	#name : #AbstractWidget,
	#superclass : #Model,
	#instVars : [
		'model'
	],
	#category : #'Nautilus-Widgets'
}

{ #category : #accessing }
AbstractWidget class >> listClass [
	^ FTPluggableIconListMorphAdaptor
]

{ #category : #'instance creation' }
AbstractWidget class >> model: model [

	^ self new
		model: model;
		yourself
]

{ #category : #'drag and drop' }
AbstractWidget >> dragTransferTypeForMorph: dragSource [ 

	^(dragSource isKindOf: self class listClass)
		ifTrue: [ dragSource getListElementSelector ]
]

{ #category : #accessing }
AbstractWidget >> hasFocus [
	^ self subclassResponsibility
]

{ #category : #accessing }
AbstractWidget >> model [

	^ model
]

{ #category : #accessing }
AbstractWidget >> model: anObject [

	model ifNotNil: [  model removeDependent: self].
	anObject ifNotNil: [  anObject addDependent:self].
	model := anObject.

]

{ #category : #accessing }
AbstractWidget >> takeKeyboardFocus [
	^ self subclassResponsibility
]

{ #category : #'drag and drop' }
AbstractWidget >> transferFor: passenger from: aMorph [

	^ TransferPresenter new
		transfer: passenger;
		from: aMorph;
		yourself
]

{ #category : #updating }
AbstractWidget >> update: aSymbol [

	self changed: aSymbol
]
