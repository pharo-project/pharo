"
SUnit tests for slices
"
Class {
	#name : #RGSliceTest,
	#superclass : #TestCase,
	#category : #Ring-Tests-Containers
}

{ #category : #testing }
RGSliceTest >> testAddingClasses [
	| newSlice newClass newTrait  |
	
	newSlice:= RGSlice named: 'Foo'.
	newClass:= RGClassDefinition named: #OrderedCollection.
	newTrait:=  RGTraitDefinition named: #TSortable.
	
	newSlice addClassNamed: #SortedCollection;
			 addClass: newClass;
			 addTrait: newTrait.
	
	self assert: (newSlice classes size = 3).
	self assert: (newSlice traits size = 1).
	self assert: (newSlice includesClassNamed: #OrderedCollection).
	self assert: ((newSlice classNamed: #OrderedCollection) == newClass).
	self assert: ((newSlice traitNamed: #TSortable) == newTrait).
	self assert: (newClass isIncludedInContainer: newSlice).
	self assert: (newTrait isIncludedInContainer: newSlice).
	self assert: (newSlice allClasses size = 3).
	self assert: (newSlice allTraits size = 1).
	self assert: (newClass parent = newSlice).
	self assert: (newClass rootEnvironment = newSlice environment)
]

{ #category : #testing }
RGSliceTest >> testNestingContainers [
	| newSlice newSlice2 newClass newTrait newClass2 |
	
	newSlice:= RGSlice named: #Root.
	newSlice2:= RGSlice named: #Child.
	newSlice2 environment: newSlice.
	
	newSlice addClassNamed: #OrderedCollection.
	newClass:= newSlice classNamed: #OrderedCollection.
	newSlice addTraitNamed: #TSortable.
	newTrait:= newSlice traitNamed: #TSortable.
	newSlice2 addClassNamed: #SortedCollection.
	newClass2:= newSlice classNamed: #SortedCollection.
	
	self assert: (newSlice classes size = 2).
	self assert: (newSlice2 classes size = 1).
	self assert: (newSlice2 environment = newSlice).
	self assert: (newSlice2 rootEnvironment = newSlice environment)
]

{ #category : #testing }
RGSliceTest >> testNewSlice [
	| newSlice |
	
	newSlice:= RGSlice named: 'Foo'.
	
	self assert: (newSlice isSlice).
	self assert: (newSlice classes isEmpty).
	self assert: (newSlice environment == Smalltalk globals)
]
