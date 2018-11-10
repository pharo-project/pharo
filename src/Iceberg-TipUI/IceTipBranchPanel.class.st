"
I'm a generic panel to checkout panels. 
My children will define different strategies to checkout (select an existing branch -local or remote-, or enter a new branch name)
"
Class {
	#name : #IceTipBranchPanel,
	#superclass : #IceTipComposablePresenter,
	#instVars : [
		'model',
		'acceptBlock'
	],
	#category : #'Iceberg-TipUI-View-Branch'
}

{ #category : #accessing }
IceTipBranchPanel class >> icon [
	^ self subclassResponsibility
]

{ #category : #'instance creation' }
IceTipBranchPanel class >> onRepository: aRepository [
	^ self on: (IceTipRepositoryModel on: aRepository) beCached
]

{ #category : #accessing }
IceTipBranchPanel class >> order [
	^ self subclassResponsibility
]

{ #category : #actions }
IceTipBranchPanel >> accept [
	self validate.
	self doAccept.
	acceptBlock ifNotNil: [ acceptBlock cull: self ]
]

{ #category : #actions }
IceTipBranchPanel >> doAccept [
	self subclassResponsibility
]

{ #category : #accessing }
IceTipBranchPanel >> icon [
	^ self class icon
]

{ #category : #testing }
IceTipBranchPanel >> isNewBranch [
	^ false
]

{ #category : #accessing }
IceTipBranchPanel >> model [
	^ model
]

{ #category : #accessing }
IceTipBranchPanel >> model: anObject [
	model := anObject.
	self refresh
]

{ #category : #events }
IceTipBranchPanel >> onAccept: aBlock [
	acceptBlock := aBlock
]

{ #category : #accessing }
IceTipBranchPanel >> refresh [
]

{ #category : #initialization }
IceTipBranchPanel >> setModelBeforeInitialization: anObject [ 
	model := anObject
]

{ #category : #accessing }
IceTipBranchPanel >> validate [

]
