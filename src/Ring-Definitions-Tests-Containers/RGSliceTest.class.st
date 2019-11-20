"
SUnit tests for slices
"
Class {
	#name : #RGSliceTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Containers-Base'
}

{ #category : #testing }
RGSliceTest >> testAddingClasses [
	| newSlice newClass newTrait |
	newSlice := RGSlice named: 'Foo'.
	newClass := RGClassDefinition named: #OrderedCollection.
	newTrait := RGTraitDefinition named: #TSortable.

	newSlice
		addClassNamed: #SortedCollection;
		addClass: newClass;
		addTrait: newTrait.

	self assert: newSlice classes size equals: 3.
	self assert: newSlice traits size equals: 1.
	self assert: (newSlice includesClassNamed: #OrderedCollection).
	self assert: (newSlice classNamed: #OrderedCollection) identicalTo: newClass.
	self assert: (newSlice traitNamed: #TSortable) identicalTo: newTrait.
	self assert: (newClass isIncludedInContainer: newSlice).
	self assert: (newTrait isIncludedInContainer: newSlice).
	self assert: newSlice allClasses size equals: 3.
	self assert: newSlice allTraits size equals: 1.
	self assert: newClass parent equals: newSlice.
	self assert: newClass rootEnvironment equals: newSlice environment
]

{ #category : #testing }
RGSliceTest >> testNestingContainers [
	| newSlice newSlice2 newClass newTrait newClass2 |
	newSlice := RGSlice named: #Root.
	newSlice2 := RGSlice named: #Child.
	newSlice2 environment: newSlice.

	newSlice addClassNamed: #OrderedCollection.
	newClass := newSlice classNamed: #OrderedCollection.
	newSlice addTraitNamed: #TSortable.
	newTrait := newSlice traitNamed: #TSortable.
	newSlice2 addClassNamed: #SortedCollection.
	newClass2 := newSlice classNamed: #SortedCollection.

	self assert: newSlice classes size equals: 2.
	self assert: newSlice2 classes size equals: 1.
	self assert: newSlice2 environment equals: newSlice.
	self assert: newSlice2 rootEnvironment equals: newSlice environment
]

{ #category : #testing }
RGSliceTest >> testNewSlice [
	| newSlice |
	newSlice := RGSlice named: 'Foo'.

	self assert: newSlice isSlice.
	self assertEmpty: newSlice classes.
	self assert: newSlice environment identicalTo: Smalltalk globals
]
