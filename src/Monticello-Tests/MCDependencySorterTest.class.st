Class {
	#name : #MCDependencySorterTest,
	#superclass : #TestCase,
	#category : #'Monticello-Tests-Dependencies'
}

{ #category : #testing }
MCDependencySorterTest class >> isUnitTest [
	^false
]

{ #category : #asserting }
MCDependencySorterTest >> assertItems: anArray orderAs: depOrder withRequired: missingDeps toLoad: unloadableItems [
	self assertItems: anArray orderAs: depOrder withRequired: missingDeps  toLoad: unloadableItems  extraProvisions: #()
]

{ #category : #asserting }
MCDependencySorterTest >> assertItems: anArray orderAs: depOrder withRequired: missingDeps toLoad: unloadableItems extraProvisions: provisions [
	| order sorter items missing unloadable |
	items := anArray collect: [ :ea | self itemWithSpec: ea ].
	sorter := MCDependencySorter items: items.
	sorter addExternalProvisions: provisions.
	order := (sorter orderedItems collect: [ :ea | ea name ]) asArray.
	self assert: order equals: depOrder.
	missing := sorter externalRequirements.
	self assert: missing asSet equals: missingDeps asSet.
	unloadable := (sorter itemsWithMissingRequirements collect: [ :ea | ea name ]) asArray.
	self assert: unloadable asSet equals: unloadableItems asSet
]

{ #category : #building }
MCDependencySorterTest >> itemWithSpec: anArray [
	^ MCMockDependentItem new
		name: anArray first;
		provides: anArray second;
		requires: anArray third
]

{ #category : #tests }
MCDependencySorterTest >> testCascadingUnresolved [
	self assertItems: #(
		(a (x) (z))
		(b () (x))
		(c () ()))
	orderAs: #(c)
	withRequired: #(z)
	toLoad: #(a b)	
]

{ #category : #tests }
MCDependencySorterTest >> testCycle [
	self assertItems: #(
		(a (x) (y))
		(b (y) (x)))
	orderAs: #()
	withRequired: #()
	toLoad: #(a b)	
]

{ #category : #tests }
MCDependencySorterTest >> testExtraProvisions [
	self assertItems:
		#((a (x) (z))
		(b () (x)))
	orderAs: #(a b)
	withRequired: #()
	toLoad: #()	
	extraProvisions: #(x z)
]

{ #category : #tests }
MCDependencySorterTest >> testMultiRequirementOrdering [
	self assertItems: #(
		(a (x) (z))
		(b (y) ())
		(c (z) ())
		(d () (x y z)))
		orderAs: #(b c a d)
		withRequired: #()
		toLoad: #()
]

{ #category : #tests }
MCDependencySorterTest >> testSimpleOrdering [
	self assertItems: #((a (x) ())
								 (c () (y))
								 (b (y) (x)))
		orderAs: #(a b c)
		withRequired: #()
		toLoad: #()
]

{ #category : #tests }
MCDependencySorterTest >> testSimpleUnresolved [
	self assertItems: #(
		(a () (z)))
	orderAs: #()
	withRequired: #(z)
	toLoad: #(a)
		
]

{ #category : #tests }
MCDependencySorterTest >> testUnusedAlternateProvider [
	self assertItems: #(
		(a (x) (z))
		(b () (x))
		(c (x) ()))
	orderAs: #(c b)
	withRequired: #(z)
	toLoad: #(a)	
]
