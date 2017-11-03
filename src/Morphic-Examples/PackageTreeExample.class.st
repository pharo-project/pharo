"
PackageTreeExample new open
"
Class {
	#name : #PackageTreeExample,
	#superclass : #MorphTreeModel,
	#category : #Morphic-Examples-MorphTreeWidget
}

{ #category : #examples }
PackageTreeExample class >> example [

	^PackageTreeExample new open
]

{ #category : #'as yet unclassified' }
PackageTreeExample >> defaultTreeMorph [
	^ super defaultTreeMorph
			doubleClickSelector: #doubleClick;
			nodeStringGetter: #asString;
			getMenuSelector: #menu:shifted:.

]

{ #category : #'as yet unclassified' }
PackageTreeExample >> doubleClick [
	self selectedNode ifNotNil: [:n | n doubleClick]
]

{ #category : #'as yet unclassified' }
PackageTreeExample >> keyStroke: anEvent from: aTreeMorph [ 
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph]
]

{ #category : #'as yet unclassified' }
PackageTreeExample >> menu: menu shifted: b [ 
	"Set up the menu to apply to the receiver's, 
	honoring the #shifted boolean"
	self selectedNode ifNotNil: [:current | current menu: menu shifted: b].
	^ menu
]

{ #category : #'as yet unclassified' }
PackageTreeExample >> open [
	^ self openDialogWindowIn: World title: 'Packages'.

]

{ #category : #'as yet unclassified' }
PackageTreeExample >> rootItems [
	^ RPackageOrganizer default packages 
]

{ #category : #'as yet unclassified' }
PackageTreeExample >> rootNodeClassFromItem: anItem [
	^ PackageNodeExample
]
