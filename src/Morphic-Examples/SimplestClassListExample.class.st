"
SimplestClassListExample new openOn: Object
"
Class {
	#name : #SimplestClassListExample,
	#superclass : #MorphTreeModel,
	#instVars : [
		'rootClass',
		'selected'
	],
	#category : #'Morphic-Examples-MorphTreeWidget'
}

{ #category : #examples }
SimplestClassListExample class >> example [

	SimplestClassListExample new openOn: Object
]

{ #category : #view }
SimplestClassListExample >> defaultTreeMorph [
	^ super defaultTreeMorph
		"beCheckList;"
		"autoDeselection: true;"
		multiSelection: true;
		autoMultiSelection: true;
		yourself
]

{ #category : #view }
SimplestClassListExample >> openDialogOn: aClass [ 
	rootClass := aClass.
	^ self openDialogWindowIn: self currentWorld title: 'All Object subclasses'

]

{ #category : #'instance creation' }
SimplestClassListExample >> openOn: aClass [ 
	rootClass := aClass.
	(self defaultTreeMorph buildContents embeddedInMorphicWindowLabeled: 'test') openInWorld
	"(self dialogWindowIn: World title: 'All Object subclasses') openInWorld"

]

{ #category : #examples }
SimplestClassListExample >> plugOne [
	| l |
	rootClass := Object.
	l := PluggableListMorph
		on: self 
		list: #rootItems
		selected: #selected
		changeSelected: #selected:.
	(l embeddedInMorphicWindowLabeled: 'test') openInWorld
]

{ #category : #accessing }
SimplestClassListExample >> rootItems [
	^ rootClass allSubclasses asArray sort: [:a :b | a name < b name]
]

{ #category : #accessing }
SimplestClassListExample >> rootItemsOrig [
	^ rootClass allSubclasses asArray sort: [:a :b | a name < b name]
]

{ #category : #accessing }
SimplestClassListExample >> rootItemsTest [
	^ (rootClass allSubclasses asArray sort: [:a :b | a name < b name])
]

{ #category : #accessing }
SimplestClassListExample >> selected [
	^ selected ifNil: [selected := 1]
]

{ #category : #accessing }
SimplestClassListExample >> selected: x [
	selected := x.
	self changed: #selected
]
