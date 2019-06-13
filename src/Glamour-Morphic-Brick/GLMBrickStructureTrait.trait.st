"
I declare api and logic to work with structure of a Brick as adding, removing, testing for existance and some more
"
Trait {
	#name : #GLMBrickStructureTrait,
	#category : #'Glamour-Morphic-Brick-Traits'
}

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> addAllBricks: aCollection [
	|bricks|
	bricks := self privateAddAllBricks: aCollection.
	bricks do: [ :b | self privatePrepareBrickAfterAdding: b ].
	self doLayoutForce.
	^ aCollection
]

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> addBrick: aBrick after: otherBrick [
	|result|
	result := self privateAddBrick: aBrick asBrick after: otherBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick
]

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> addBrick: aBrick before: otherBrick [
	|result|
	result := self privateAddBrick: aBrick asBrick before: otherBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick
]

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> addBrickBack: aBrick [
	|result|
	result := self privateAddBrickBack: aBrick asBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick
]

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> addBrickFirst: aBrick [
	|result|
	result := self privateAddBrickFirst: aBrick asBrick.
	self privatePrepareBrickAfterAdding: result.
	self doLayoutForce.
	^ aBrick
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> allOwnersDo: aBlock [
	"Evaluate aBlock with all owners of the receiver"
	self owner ifNotNil:[ ^ self owner withAllOwnersDo: aBlock ].
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> allSubbricks [
	| all |
	all := OrderedCollection new.
	self allSubbricksDo: [:each | all add: each ].
	^ all
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> allSubbricksDo: aBlock [

	self subbricks do: [:each | 
		each isBrick ifTrue: [ aBlock value: each. each allSubbricksDo: aBlock ] ]
]

{ #category : #'brick-structure-converting' }
GLMBrickStructureTrait >> asBrick [
	^ self
]

{ #category : #'brick-structure-converting' }
GLMBrickStructureTrait >> asBrickedMorph [

	^ GLMFoundationBrick new brickRoot: self
]

{ #category : #'brick-structure-converting' }
GLMBrickStructureTrait >> asFormValue [

	^ nil
]

{ #category : #'brick-structure-converting' }
GLMBrickStructureTrait >> brick [

	^ self
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> brickDoubleLink [

	^ self valueOfProperty: #brickDoubleLink ifAbsentPut: [ nil ]
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> brickDoubleLink: aDoubleLink [

	self setProperty: #brickDoubleLink toValue: aDoubleLink
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> brickRoot [

	^ self owner ifNotNil: [ self owner brickRoot ] ifNil: [ ^ self ]
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> firstOwnerSuchThat: conditionBlock [

	self allOwnersDo: [:m | (conditionBlock cull: m) ifTrue: [^ m]].
	^ nil

]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> firstSubbrick [

	^ self subbricks first
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> hasOwner [

	^ self owner isNotNil
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> hasSubbrick: aBrick [

	^ aBrick owner = self
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> hasSubbricks [

	^ self subbricks isEmpty not
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> isBrick [

	^ true
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> isFirstInOwner [

	^ self brickDoubleLink 
		ifNotNil: [ :bdl | bdl hasPredecessor not ]
		ifNil: [ false ]
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> isFoundationBrick [

	^ false
]

{ #category : #'brick-structure-testing' }
GLMBrickStructureTrait >> isLastInOwner [

	^ self brickDoubleLink
		ifNotNil: [ :bdl | bdl hasSuccessor not ]
		ifNil: [ false ]
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> lastSubbrick [

	^ self subbricks last
]

{ #category : #'brick-structure-converting' }
GLMBrickStructureTrait >> morph [
	^ self
]

{ #category : #'brick-structure-events' }
GLMBrickStructureTrait >> onRemovedFrom: aBrick [
	"is called when brick is removed from aBrick"
	self subbricks do: [ :each | each onRemovedFrom: aBrick ]
]

{ #category : #'brick-structure-opening' }
GLMBrickStructureTrait >> openCenteredInBrickWindow [

	self openCenteredInBrickWindowLabeled: self
]

{ #category : #'brick-structure-opening' }
GLMBrickStructureTrait >> openCenteredInBrickWindowLabeled: aLabel [

	self
		vAlign: #center;
		hAlign: #center;
		openInBrickWindowLabeled: aLabel
]

{ #category : #'brick-structure-opening' }
GLMBrickStructureTrait >> openInBrickWindow [

	^ self openInBrickWindowLabeled: self
]

{ #category : #'brick-structure-opening' }
GLMBrickStructureTrait >> openInBrickWindowLabeled: aLabel [
	
	^ self class environment at: #GLMSystemWindowBrick 
		ifPresent: [ :glmSystemWindowBrick |
			glmSystemWindowBrick new
				label: aLabel;
				color: Color transparent;
				addBrickBack: (
					GLMBrick new
						vSpaceFill
						hSpaceFill;
						addBrickBack: self;
						yourself);
				extent: (400@400) * self currentWorld displayScaleFactor;
				openCenteredInWorld ]
		ifAbsent: [ self asMorph openInWindow ] 
	
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> owner [

	^ self explicitRequirement
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> owner: aBrick [
	|oldOwner|
	oldOwner := self owner.
	self privateOwner: aBrick.
	aBrick
		ifNotNil: [ self themerChanged ]
		ifNil: [ self onRemovedFrom: oldOwner ]
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> ownerThatIsA: aClass [
	"Return the first enclosing morph that is a kind of aClass, or nil if none"

	^ self firstOwnerSuchThat: [:m | m isKindOf: aClass]
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddAllBricks: aCollection [
	|bricks|
	bricks := aCollection collect: #asBrick.
	bricks do: [ :each | self privateAddBrickLinkBack: each ].
	self privateSubbricks: (self subbricks, bricks).
	^ bricks
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddBrick: aBrick after: otherBrick [
	| brick link otherLink otherLinkSuccessor index|
	brick := aBrick asBrick.
	
	otherLink := otherBrick brickDoubleLink.
	otherLink = self lastSubbrick brickDoubleLink
		ifTrue: [ ^ self privateAddBrickBack: aBrick ].
		
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	otherLinkSuccessor := otherLink nextLink.
	otherLink nextLink: link.
	link previousLink: otherLink.
	link nextLink: otherLinkSuccessor.
	otherLinkSuccessor previousLink: link.
	
	index := (self subbricks indexOf: otherBrick) + 1.
	self privateSubbricks: (self subbricks copyReplaceFrom: index to: index - 1 with: (Array with: brick)).
	
	brick owner: self.
	
	^ aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddBrick: aBrick before: otherBrick [
	| brick link otherLink otherLinkPredeccessor index|
	brick := aBrick asBrick.
	
	otherLink := otherBrick brickDoubleLink.
	otherLink = self firstSubbrick brickDoubleLink
		ifTrue: [ ^ self privateAddBrickFirst: aBrick ].
		
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	otherLinkPredeccessor := otherLink previousLink.
	otherLink previousLink: link.
	link nextLink: otherLink.
	link previousLink: otherLinkPredeccessor.
	otherLinkPredeccessor nextLink: link.
	
	index := ((self subbricks indexOf: otherBrick) max: 1).
	self privateSubbricks: (self subbricks copyReplaceFrom: index to: index - 1 with: (Array with: brick)).
	
	brick owner: self.
	
	^ aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddBrickBack: aBrick [
	|brick|
	
	brick := aBrick asBrick.
	self privateAddBrickLinkBack: brick.
	self privateSubbricks: (self subbricks copyWith: brick).
	
	^ aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddBrickFirst: aBrick [
	| link head tail brick|
	
	brick := aBrick asBrick.
	head := nil.
	tail := nil.
	self hasSubbricks ifTrue: [
		head := self firstSubbrick brickDoubleLink.
		tail := self lastSubbrick brickDoubleLink ].
	
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	link nextLink: head.
	head ifNotNil: [ head previousLink: link ].
	
	self privateSubbricks: (self subbricks copyWithFirst: brick).
	brick owner: self.
	
	^ aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddBrickLinkBack: aBrick [
	|link brick|
	
	brick := aBrick asBrick.
	link := brick asDoubleLink.
	brick brickDoubleLink: link.
	
	self hasSubbricks ifTrue: [
		self lastSubbrick brickDoubleLink nextLink: link.	
		link previousLink: self lastSubbrick brickDoubleLink ].
	
	brick owner: self.
	
	^ aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateAddIntoWorld: aBrick [

	self world == aBrick world
		ifFalse: [ self privateRemoveOutWorld: aBrick ].
	aBrick intoWorld: self world
	
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateOwner: aBrick [

	^ self explicitRequirement
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privatePrepareBrickAfterAdding: aBrick [

	aBrick wrappedBounds markWidthDirty; markHeightDirty.
	self privateAddIntoWorld: aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privatePrepareBrickAfterRemoving: aBrick [

	self privateRemoveOutWorld: aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateRemoveAllBricks [
	
	self hasSubbricks ifFalse: [ ^ self ].
	self subbricks do: [:each |
		each brickDoubleLink: nil.
		each owner: nil ].
	self subbricks removeAll
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateRemoveBrick: aBrick [
	| predecessor successor link head tail index|
	aBrick ifNil: [ ^ aBrick ].
	(self hasSubbrick: aBrick) ifFalse: [ ^ aBrick ].
	
	link := aBrick brickDoubleLink.
	
	head := self firstSubbrick brickDoubleLink.
	tail := self lastSubbrick brickDoubleLink.
	
	predecessor := link previousLink.
	successor := link nextLink.
	predecessor
		ifNotNil: [ predecessor nextLink: successor ].
	successor
		ifNotNil: [ successor previousLink: predecessor ].
	link clearLinks.
	
	aBrick brickDoubleLink: nil.
	
	index := self subbricks find: aBrick.
	self privateSubbricks: (self subbricks copyWithoutIndex: index).
	aBrick owner: nil.
	
	^ aBrick
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateRemoveOutWorld: aBrick [

	aBrick outOfWorld: aBrick world
	
]

{ #category : #'private-structure' }
GLMBrickStructureTrait >> privateSubbricks: aCollection [

	^ self explicitRequirement
]

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> removeAllBricks [
	|copy|
	copy := self subbricks copy.
	self privateRemoveAllBricks.
	copy do: [ :b | self privatePrepareBrickAfterRemoving: b ].
	self doLayoutForce
]

{ #category : #'brick-structure-adding/removing' }
GLMBrickStructureTrait >> removeBrick: aBrick [
	|result|
	aBrick ifNil: [ ^ aBrick ].
	result := self privateRemoveBrick: aBrick.
	self	privatePrepareBrickAfterRemoving: aBrick.
	self doLayoutForce.
	^ result
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> subbrickAfter [

	"Return the subbbrick after (behind) me, or nil"
	| link |
	link := self brickDoubleLink.
	link ifNil: [ ^ nil ].
	link nextLink ifNil: [ ^ nil ].
	^ link nextLink value
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> subbrickBefore [

	"Return the subbrick before me, or nil"
	| link |
	link := self brickDoubleLink.
	link ifNil: [ ^ nil ].
	link previousLink ifNil: [ ^ nil ].
	^ link previousLink value
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> subbricks [

	^ self explicitRequirement
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> subbricksCount [

	^ self subbricks size
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> subbricksReverseDo: aBlock [

	self subbricks reverseDo: aBlock
]

{ #category : #'brick-structure-opening' }
GLMBrickStructureTrait >> window [
	"Answer the receiver's window."
	
	^ self class environment at: #GLMWindowBrick 
		ifPresent: [ :glmWindowBrick |
			(self ownerThatIsA: glmWindowBrick)
				ifNil: [ self ownerThatIsA: SystemWindow ]]
		ifAbsent: [ self ownerThatIsA: SystemWindow ].
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> withAllOwnersDo: aBlock [
	"Evaluate aBlock with the receiver and all of its owners"
	aBlock cull: self.
	self owner ifNotNil:[ ^ self owner withAllOwnersDo: aBlock ]
]

{ #category : #'brick-structure' }
GLMBrickStructureTrait >> withAllSubbricks [

	^ self allSubbricks add: self;
		 yourself
]
