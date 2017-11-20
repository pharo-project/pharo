"
I am the model to manage packages selection to add them in groups
"
Class {
	#name : #PackageCreatorTreeModel,
	#superclass : #FilteredMorphTreeModel,
	#instVars : [
		'groups',
		'selectedElements'
	],
	#category : #Nautilus-GroupManagerUI
}

{ #category : #'drag and drop' }
PackageCreatorTreeModel >> acceptDroppingMorph: transferMorph event: evt inMorph: listMorph [
	patternModel acceptEditsInView
]

{ #category : #private }
PackageCreatorTreeModel >> add: aCollection into: aGroup [
	aGroup addPackages: aCollection
]

{ #category : #view }
PackageCreatorTreeModel >> defaultTreeMorph [
	^ super defaultTreeMorph
		dragEnabled: true;
		nodeStringGetter: #name
]

{ #category : #'drag and drop' }
PackageCreatorTreeModel >> dragTransferType [
	^ #SelectedPackage
]

{ #category : #'drag and drop' }
PackageCreatorTreeModel >> dragTransferTypeForMorph: listMorph [
	^ self dragTransferType
]

{ #category : #accessing }
PackageCreatorTreeModel >> groups [
	^ groups
]

{ #category : #accessing }
PackageCreatorTreeModel >> groups: anObject [
	groups := anObject
]

{ #category : #initialization }
PackageCreatorTreeModel >> initialize [
	super initialize.
	selectedElements := OrderedCollection new
]

{ #category : #display }
PackageCreatorTreeModel >> open [
	(DialogGroupManagerUI on: self)
		title: 'Group Manager' translated;
		openInWorld
]

{ #category : #accessing }
PackageCreatorTreeModel >> patternModel [
	^ patternModel
]

{ #category : #private }
PackageCreatorTreeModel >> remove: aCollection from: aGroup [
	aGroup removePackages: aCollection
]

{ #category : #accessing }
PackageCreatorTreeModel >> selectedElements [
	^ selectedElements
]

{ #category : #accessing }
PackageCreatorTreeModel >> selectedElements: anObject [
	selectedElements := anObject
]

{ #category : #'drag and drop' }
PackageCreatorTreeModel >> wantsDroppedMorph: transferMorph event: evt inMorph: listMorph [
	(transferMorph isKindOf: TransferMorph)
		ifFalse: [ ^ false ].
	^ true
]
