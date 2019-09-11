"
PackageTreeExample new open
"
Class {
	#name : #PackageTreeExample,
	#superclass : #MorphTreeModel,
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #examples }
PackageTreeExample class >> example [
	<sampleInstance>

	^PackageTreeExample new open
]

{ #category : #view }
PackageTreeExample >> defaultTreeMorph [
	^ super defaultTreeMorph
			doubleClickSelector: #doubleClick;
			nodeStringGetter: #asString;
			getMenuSelector: #menu:shifted:.

]

{ #category : #'event handling' }
PackageTreeExample >> doubleClick [
	self selectedNode ifNotNil: [:n | n doubleClick]
]

{ #category : #accessing }
PackageTreeExample >> keyStroke: anEvent from: aTreeMorph [ 
	self selectedNode
		ifNotNil: [:current | current keyStroke: anEvent from: aTreeMorph]
]

{ #category : #accessing }
PackageTreeExample >> menu: menu shifted: b [ 
	"Set up the menu to apply to the receiver's, 
	honoring the #shifted boolean"
	self selectedNode ifNotNil: [:current | current menu: menu shifted: b].
	^ menu
]

{ #category : #showing }
PackageTreeExample >> open [
	^ self openDialogWindowIn: self currentWorld title: 'Packages'.

]

{ #category : #accessing }
PackageTreeExample >> rootItems [
	^ RPackageOrganizer default packages 
]

{ #category : #accessing }
PackageTreeExample >> rootNodeClassFromItem: anItem [
	^ PackageNodeExample
]
