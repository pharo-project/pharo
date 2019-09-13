Class {
	#name : #GLMBrickTest,
	#superclass : #TestCase,
	#instVars : [
		'layouter'
	],
	#category : #'Glamour-Morphic-Brick-Tests'
}

{ #category : #asserting }
GLMBrickTest >> assertAllSubbricksAreClean: aBrick [

	aBrick withAllSubbricks do: [ :each | self assertBrickClean: each ]
]

{ #category : #asserting }
GLMBrickTest >> assertAllSubbricksAreDirty: aBrick [

	aBrick withAllSubbricks do: [ :each | self assertBrickDirty: each ]
]

{ #category : #asserting }
GLMBrickTest >> assertBrickClean: aBrick [

	self assertWidthClean: aBrick.
	self assertHeightClean: aBrick
]

{ #category : #asserting }
GLMBrickTest >> assertBrickDirty: aBrick [

	self assertWidthDirty: aBrick.
	self assertHeightDirty: aBrick
]

{ #category : #asserting }
GLMBrickTest >> assertHeightClean: aBrick [

	self assert: aBrick wrappedBounds heightState equals: #clean.
	self assert: aBrick wrappedBounds isHeightDirty not.
	self assert: aBrick wrappedBounds isHeightClean
]

{ #category : #asserting }
GLMBrickTest >> assertHeightDirty: aBrick [

	self assert: aBrick wrappedBounds heightState equals: #dirty.
	self assert: aBrick wrappedBounds isHeightDirty.
	self assert: aBrick wrappedBounds isHeightClean not
]

{ #category : #asserting }
GLMBrickTest >> assertWidthClean: aBrick [

	self assert: aBrick wrappedBounds widthState equals: #clean.
	self assert: aBrick wrappedBounds isWidthDirty not.
	self assert: aBrick wrappedBounds isWidthClean
]

{ #category : #asserting }
GLMBrickTest >> assertWidthDirty: aBrick [

	self assert: aBrick wrappedBounds widthState equals: #dirty.
	self assert: aBrick wrappedBounds isWidthDirty.
	self assert: aBrick wrappedBounds isWidthClean not
]

{ #category : #'instance creation' }
GLMBrickTest >> markFullyClean: aBrick [ 

	aBrick withAllSubbricks do: [ :each | each wrappedBounds markWidthClean; markHeightClean ].
	^ aBrick
]

{ #category : #'instance creation' }
GLMBrickTest >> newBrickFullHeightAndWidthDependsOnParent: anInteger [

	^ GLMBrick new
		vSpaceFill;
		hSpaceFill: anInteger;
		color: Color random;
		yourself
]

{ #category : #'instance creation' }
GLMBrickTest >> newBrickWithFullDependsOnChildren [

	^ GLMBrick new 
		hShrinkWrap;
		vShrinkWrap;
		color: Color random;
		yourself
]

{ #category : #'instance creation' }
GLMBrickTest >> newBrickWithFullFill [

	^ GLMBrick new 
		hSpaceFill;
		vSpaceFill;
		color: Color random;
		yourself
]

{ #category : #'instance creation' }
GLMBrickTest >> newBrickWithSmallStaticExtent [

	^ GLMBrick new 
		height: self staticSmallHeight;
		width: self staticSmallWidth;
		color: Color random;
		yourself
]

{ #category : #'instance creation' }
GLMBrickTest >> newBrickWithStaticExtent [

	^ GLMBrick new 
		height: self staticHeight;
		width: self staticWidth;
		color: Color random;
		yourself
]

{ #category : #running }
GLMBrickTest >> setUp [
	super setUp.
	
	layouter := GLMBrickLayouter new
]

{ #category : #defaults }
GLMBrickTest >> staticExtent [

	^ self staticWidth  @ self staticHeight
]

{ #category : #defaults }
GLMBrickTest >> staticHeight [

	^ 100 * self currentWorld displayScaleFactor
]

{ #category : #defaults }
GLMBrickTest >> staticSmallExtent [

	^ self staticSmallWidth  @ self staticSmallHeight
]

{ #category : #defaults }
GLMBrickTest >> staticSmallHeight [

	^ 50 * self currentWorld displayScaleFactor
]

{ #category : #defaults }
GLMBrickTest >> staticSmallWidth [

	^ 50 * self currentWorld displayScaleFactor
]

{ #category : #defaults }
GLMBrickTest >> staticWidth [

	^ 100 * self currentWorld displayScaleFactor
]

{ #category : #'tests-layouter' }
GLMBrickTest >> test100DependsOnChildrenBricksInsideOneStatic [
	|root current|
	
	root := self newBrickWithFullDependsOnChildren.
	current := root.
	1 to: 99 do: [ :index |
		current := current addBrickBack: self newBrickWithFullDependsOnChildren ].
	current := current addBrickBack: self newBrickWithStaticExtent.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: current extent
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> test100FillBricksInsideEachOtherIndideStatic [
	|root current|
	
	root := self newBrickWithStaticExtent.
	current := root.
	1 to: 100 do: [ :index |
		current := current addBrickBack: self newBrickWithFullFill ].
	
	self assertAllSubbricksAreClean: root.
	
	self assert: current extent equals: root extent
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> test100StaticBricksInsideEachOtherIndideStatic [
	|root current|
	
	root := self newBrickWithStaticExtent.
	current := root.
	1 to: 100 do: [ :index |
		current := current addBrickBack: self newBrickWithStaticExtent ].
	
	self assertAllSubbricksAreClean: root.
	
	self assert: current extent equals: root extent
	
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testBottomMarginMyselfDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginBottom: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallExtent)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testBottomMarginParentDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick marginBottom: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: (self staticExtent - (0@margin)).
	self assert: brick brickBounds equals: (0@0 corner: self staticExtent - (0@margin))
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testBrickDependsOnChildrenOneStatic [
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	
	root addBrickBack: brick.
	
	self assert: root brickApi layout hState isOnChildren.
	self assert: root brickApi layout vState isOnChildren.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: brick extent
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testChangeChildrenInParent [
	|parent brick|
	parent := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	
	parent addBrickBack: brick.
	self assertAllSubbricksAreClean: parent.
	self assert: parent extent equals: brick extent.
	"height and width are changed, so parent should be re-layouted"
	brick height: self staticHeight * 2.
	brick width: self staticWidth * 2.
	
	self assertAllSubbricksAreClean: parent.
	self assert: brick extent equals: self staticExtent * 2.
	self assert: parent extent equals: brick extent
	

]

{ #category : #'tests-layouter' }
GLMBrickTest >> testChangeChildrenInParentAndSubchildren [
	|parent brick child|
	parent := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	child := self newBrickFullHeightAndWidthDependsOnParent: 100.
	
	parent addBrickBack: brick.
	brick addBrickBack: child.
	self assertAllSubbricksAreClean: parent.
	self assert: parent extent equals: brick extent.
	self assert: child extent equals: brick extent.
	"height and width are changed, so parent should be re-layouted"
	brick height: self staticHeight * 2.
	brick width: self staticWidth * 2.
	
	self assertAllSubbricksAreClean: parent.
	self assert: brick extent equals: self staticExtent * 2.
	self assert: parent extent equals: brick extent.
	self assert: child extent equals: brick extent
	

]

{ #category : #'tests-layouter' }
GLMBrickTest >> testEmptyBrickDependsOnChildren [
	|brick|
	
	brick := self newBrickWithFullDependsOnChildren.
	
	self assert: brick brickApi layout hState isOnChildren.
	self assert: brick brickApi layout vState isOnChildren.
	
	self assertAllSubbricksAreDirty: brick.
	brick doLayout.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick extent equals: 0@0
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testFillBrickInsideStatic [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithFullFill.
	brick brickApi layout hState isOnParent.
	brick brickApi layout vState isOnParent.
	root addBrickBack: brick.
	
	self assertAllSubbricksAreClean: root.
	self assert: brick extent equals: root extent
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testFreshBrickIsDirty [

	self assertBrickDirty: GLMBrick new
]

{ #category : #'tests-align' }
GLMBrickTest >> testFullAlignCenterShrinkWrapInSpaceFill [
	|root brick1 brick2 brick3 increment|
	
	increment := 10.
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickWithFullFill.
	brick2 := self newBrickWithFullDependsOnChildren.
	brick3 := self newBrickWithSmallStaticExtent.
	
	brick2
		vAlign: #center;
		hAlign: #center.
		
	root addBrickBack: brick1.
	brick1 addBrickBack: brick2.
	brick2 addBrickBack: brick3.
	
	self assertAllSubbricksAreClean: root.
	self assert: brick2 brickBounds equals: ((self staticExtent - self staticSmallExtent / 2) extent: self staticSmallExtent) ceiling.
	
	brick3
		height: brick3 height + increment;
		width: brick3 width + increment.
	self assertAllSubbricksAreClean: root.
	
	self assert: brick2 brickBounds equals: ((self staticExtent - self staticSmallExtent / 2 - (increment / 2) ) extent: (self staticSmallExtent + increment)) ceiling.
	
	
	
	
]

{ #category : #'tests-floating' }
GLMBrickTest >> testLeftFloatingMySelfDependency [
	|root floating brick|
	floating := #left.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick layout floating: floating.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallExtent)
]

{ #category : #'tests-floating' }
GLMBrickTest >> testLeftFloatingMySelfDependencyWithMargin [
	|root floating margin brick|
	margin := 10.
	floating := #left.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginLeft: margin.
	brick layout floating: floating.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (margin@0 extent: self staticSmallExtent)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testLeftMarginChangeMyselfDependency [
	|root margin newMargin brick|
	margin := 10.
	newMargin := 20.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginLeft: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (margin@0 extent: self staticSmallExtent).
	
	brick marginLeft: newMargin.
	brick doLayoutForce.
	
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (newMargin@0 extent: self staticSmallExtent).
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testLeftMarginMyselfDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginLeft: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (margin@0 extent: self staticSmallExtent)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testLeftMarginParentDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick marginLeft: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: (self staticExtent - (margin@0)).
	self assert: brick brickBounds equals: (margin@0 corner: self staticExtent)
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenHeight [
	|brick|
	
	brick := self newBrickWithFullDependsOnChildren.
	self markFullyClean: brick.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenInChildrenHeight [
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightDirty.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenInChildrenWidth [
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthDirty.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenInParentHeight [
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenInParentWidth [
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenInStaticHeight [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenInStaticWidth [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithFullDependsOnChildren.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneChildrenWidth [
	|brick|
	
	brick := self newBrickWithFullDependsOnChildren.
	self markFullyClean: brick.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneParentHeight [
	|brick|
	
	brick := self newBrickWithFullFill.
	self markFullyClean: brick.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneParentInParentHeight [
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithFullFill.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneParentInParentWidth [
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithFullFill.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneParentInStaticHeight [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithFullFill.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneParentInStaticWidth [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithFullFill.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneParentWidth [
	|brick|
	
	brick := self newBrickWithFullFill.
	self markFullyClean: brick.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticHeight [
	|brick|
	
	brick := self newBrickWithStaticExtent.
	self markFullyClean: brick.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticInChildrenHeight [
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightDirty.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticInChildrenWidth [
	|root brick|
	
	root := self newBrickWithFullDependsOnChildren.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthDirty.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticInParentHeight [
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticInParentWidth [
	|root brick|
	
	root := self newBrickWithFullFill.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticInStaticHeight [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markHeightDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthClean.
	self assert: brick wrappedBounds isHeightDirty.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticInStaticWidth [
	|root brick|
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithStaticExtent.
	root addBrickBack: brick.
	self markFullyClean: root.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
	
	self assert: root wrappedBounds isWidthClean.
	self assert: root wrappedBounds isHeightClean.
]

{ #category : #'tests-marker' }
GLMBrickTest >> testMarkerOneStaticWidth [
	|brick|
	
	brick := self newBrickWithStaticExtent.
	self markFullyClean: brick.
	
	brick wrappedBounds markWidthDirty.
	GLMBrickLayouter uniqueInstance markDependent: brick brickApi.
	
	self assert: brick wrappedBounds isWidthDirty.
	self assert: brick wrappedBounds isHeightClean.
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMaxHeightOneStatic [
	|brick|
	
	brick := self newBrickWithStaticExtent.
	brick maxHeight: self staticSmallHeight.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticWidth @ self staticSmallHeight)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMaxHeightTwoParentDependencies [
	|root brick1 brick2|
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 maxHeight: self staticSmallHeight.
	brick2 := self newBrickWithFullFill.
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ self staticSmallHeight).
	self assert: brick2 extent equals: (self staticWidth @ (self staticHeight - self staticSmallHeight))
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMaxWidthOneStatic [
	|brick|
	
	brick := self newBrickWithStaticExtent.
	brick maxWidth: self staticSmallWidth.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallWidth @ self staticHeight)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMinAndMaxHeightTwoParentDependencies [
	|root brick1 brick2 minHeight maxHeight|
	minHeight := 60.
	maxHeight := 20.
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 vSpaceFill: 50. "50% = 50px"
	brick1 minHeight: minHeight. "min is 60px > 50%"
	brick2 := self newBrickWithFullFill.
	brick2 maxHeight: maxHeight. "max is 20 < 100%"
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ minHeight).
	self assert: brick2 extent equals: (self staticWidth @ maxHeight)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMinAndMaxHeightTwoParentDependenciesReverse [
	|root brick1 brick2 minHeight maxHeight|
	minHeight := 20.
	maxHeight := 60.
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 vSpaceFill: 0. "0% = 0px"
	brick1 minHeight: minHeight. "min is 20px > 0%"
	brick2 := self newBrickWithFullFill.
	brick2 maxHeight: maxHeight. "max is 60 < 100%"
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ minHeight).
	self assert: brick2 extent equals: (self staticWidth @ maxHeight)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMinHeightOneStatic [
	|brick|
	
	brick := self newBrickWithSmallStaticExtent.
	brick minHeight: self staticHeight.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallWidth @ self staticHeight)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMinHeightTwoParentDependencies [
	|root brick1 brick2 height|
	height := 80.
	
	root := self newBrickWithStaticExtent.
	root useVerticalLinearLayout.
	brick1 := self newBrickWithFullFill.
	brick1 vSpaceFill: 50. "50% = 50px"
	brick1 minHeight: height. "min is 80px > 50%"
	brick2 := self newBrickWithFullFill.
	
	root addAllBricks:  { brick1 . brick2 }.
	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 extent equals: (self staticWidth @ height).
	self assert: brick2 extent equals: (self staticWidth @ (self staticHeight - height))
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testMinWidthOneStatic [
	|brick|
	
	brick := self newBrickWithSmallStaticExtent.
	brick minWidth: self staticWidth.
	brick doLayoutForce.
	self assertAllSubbricksAreClean: brick.
	
	self assert: brick brickBounds equals: (0@0 extent: self staticWidth @ self staticSmallHeight)
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testOneDependsOnParentOneDependsOnChildrenInsideStatic [
	|root brick1 brick2 brick3 staticWidth staticHeight|
	
	staticWidth := 20.
	staticHeight := 50.
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick2 := self newBrickWithFullDependsOnChildren.
	brick3 := GLMBrick new width: staticWidth; height: staticHeight; yourself.
	
	root addBrickBack: brick1.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: self staticExtent).
	
	root addBrickBack: brick2.
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: self staticExtent).
	self assert: brick2 brickBounds equals: (self staticWidth @0 extent: 0@0).
	
	brick2 addBrickBack: brick3.
		
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth - staticWidth) @ self staticHeight).
	self assert: brick2 brickBounds equals: ((self staticWidth - staticWidth)@0 extent: (staticWidth) @ staticHeight).
	self assert: brick3 brickBounds equals: (0@0 extent: (staticWidth) @ staticHeight).
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testOneDependsOnParentOneStaticInsideStatic [
	|root brick1 brick2 staticWidth|
	
	staticWidth := 20.
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick2 := GLMBrick new width: staticWidth; vSpaceFill; yourself.
	
	root
		addBrickBack: brick1;
		addBrickBack: brick2.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth - staticWidth) @ self staticHeight).
	self assert: brick2 brickBounds equals: ((self staticWidth - staticWidth)@0 extent: (staticWidth) @ self staticHeight).
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testOneStaticBrick [
	|brick|
	
	brick := self newBrickWithStaticExtent.
	
	self assert: brick brickApi layout hState isOnMyself.
	self assert: brick brickApi layout vState isOnMyself.
	self assertBrickClean: brick.
	self assert: brick extent equals: self staticExtent
	
]

{ #category : #'tests-floating' }
GLMBrickTest >> testRightFloatingMySelfDependency [
	|root floating brick|
	floating := #right.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick layout floating: floating.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: ((self staticWidth - self staticSmallWidth)@0 extent: self staticSmallExtent)
]

{ #category : #'tests-floating' }
GLMBrickTest >> testRightFloatingMySelfDependencyWithMargin [
	|root margin floating brick|
	margin := 10.
	floating := #right.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick layout floating: floating.
	brick marginRight: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: ((self staticWidth - margin - self staticSmallWidth)@0 extent: self staticSmallExtent)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testRightMarginMyselfDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginRight: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@0 extent: self staticSmallExtent)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testRightMarginParentDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick marginRight: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: (self staticExtent - (margin@0)).
	self assert: brick brickBounds equals: (0@0 corner: self staticExtent - (margin@0))
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testShrinkInShrinkChange [
	|root brick1 brick2|
	
	root := self newBrickWithFullDependsOnChildren.
	brick1 := self newBrickWithFullDependsOnChildren.
	brick2 := self newBrickWithSmallStaticExtent.
	
	root addBrickBack: brick1.
	brick1 addBrickBack: brick2.
	
	self assertAllSubbricksAreClean: root.
	self assert: root brickBounds equals: (0@0 extent: self staticSmallExtent).
	
	brick2
		width: self staticWidth;
		height: self staticHeight.
	
	self assert: root brickBounds equals: (0@0 extent: self staticExtent).
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testShrinkWrapInStaticWithNeighbours [
	|root band child1 child2 neighbour childWidth width|
	width := 20.
	childWidth := self staticWidth - width.
	
	root := self newBrickWithStaticExtent useHorizontalLinearLayout.
	band := GLMBrick new useVerticalLinearLayout; hSpaceFill; vShrinkWrap.
	child1 := GLMBrick new height: self staticHeight; hSpaceFill.
	child2 := GLMBrick new height: self staticHeight; hSpaceFill.
	neighbour := GLMBrick new vSpaceFill; width: width.
	
	root addAllBricks: { band. neighbour }.
	band addAllBricks: { child1. child2 }.
	
	self assertAllSubbricksAreClean: root.
	
	self assert: root brickBounds equals: (0@0 extent: self staticExtent).
	self assert: neighbour brickBounds equals: (childWidth@0 extent: width@self staticHeight).
	self assert: child1 brickBounds equals: (0@0 extent: childWidth@self staticHeight).
	self assert: child2 brickBounds equals: (0@self staticHeight extent: childWidth@self staticHeight).
	self assert: band brickBounds equals: (0@0 extent: childWidth@(self staticHeight+self staticHeight)).
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testTopMarginMyselfDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickWithSmallStaticExtent.
	brick marginTop: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: self staticSmallExtent.
	self assert: brick brickBounds equals: (0@margin extent: self staticSmallExtent)
]

{ #category : #'tests-geometry' }
GLMBrickTest >> testTopMarginParentDependency [
	|root margin brick|
	margin := 10.
	
	root := self newBrickWithStaticExtent.
	brick := self newBrickFullHeightAndWidthDependsOnParent: 100.
	brick marginTop: margin.
	root addBrickBack: brick.

	self assertAllSubbricksAreClean: root.
	self assert: root extent equals: self staticExtent.
	self assert: brick extent equals: (self staticExtent - (0@margin)).
	self assert: brick brickBounds equals: (0@margin corner: self staticExtent)
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testTwoDependsOnParentInsideStatic [
	|root brick1 brick2|
	
	root := self newBrickWithStaticExtent.
	brick1 := self newBrickFullHeightAndWidthDependsOnParent: 50.
	brick2 := self newBrickFullHeightAndWidthDependsOnParent: 50.
	
	root
		addBrickBack: brick1;
		addBrickBack: brick2.

	self assertAllSubbricksAreClean: root.
	
	self assert: root extent equals: self staticExtent.
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth / 2) @ self staticHeight).
	self assert: brick2 brickBounds equals: ((self staticWidth / 2)@0 extent: (self staticWidth / 2) @ self staticHeight).
	
]

{ #category : #'tests-layouter' }
GLMBrickTest >> testTwoShrinkInShrinkChange [
	|root brick1 brick2 brick3|
	
	root := self newBrickWithFullDependsOnChildren.
	brick1 := self newBrickWithFullDependsOnChildren.
	brick2 := self newBrickWithSmallStaticExtent.
	brick3 := self newBrickWithSmallStaticExtent.
	brick1 useVerticalLinearLayout.
	
	root addBrickBack: brick1.
	brick1 addBrickBack: brick2.
	brick1 addBrickBack: brick3.
	
	self assertAllSubbricksAreClean: root.
	self assert: root brickBounds equals: (0@0 extent: (self staticSmallWidth @ (2* self staticSmallHeight))).
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticSmallWidth @ (2* self staticSmallHeight))).
	self assert: brick2 brickBounds equals: (0@0 extent: self staticSmallExtent).
	self assert: brick3 brickBounds equals: (0@self staticSmallHeight extent: self staticSmallExtent).
	
	brick2
		width: self staticWidth;
		height: self staticHeight.
	
	self assert: root brickBounds equals: (0@0 extent: (self staticWidth @ (self staticHeight + self staticSmallHeight))).
	self assert: brick1 brickBounds equals: (0@0 extent: (self staticWidth @ (self staticHeight + self staticSmallHeight))).
	self assert: brick2 brickBounds equals: (0@0 extent: self staticExtent).
	self assert: brick3 brickBounds equals: (0@self staticHeight extent: self staticSmallExtent).
	
]
