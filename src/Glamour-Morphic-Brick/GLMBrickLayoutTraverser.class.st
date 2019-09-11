Class {
	#name : #GLMBrickLayoutTraverser,
	#superclass : #Object,
	#category : #'Glamour-Morphic-Brick-Layouts-Utils'
}

{ #category : #private }
GLMBrickLayoutTraverser >> customDependencies: aBrick [

	^ self subclassResponsibility
]

{ #category : #private }
GLMBrickLayoutTraverser >> dimensionPolicy: aBrick [

	^ (self stateOf: aBrick) layoutPolicy: aBrick on: self
]

{ #category : #private }
GLMBrickLayoutTraverser >> hasCustomDependencies: aBrick [

	^ (self customDependencies: aBrick) isEmpty not
]

{ #category : #private }
GLMBrickLayoutTraverser >> hasDimensionRestrictions: aBrick [

	^ (self hasMinDimensionRestriction: aBrick) | (self hasMaxDimensionRestriction: aBrick)
]

{ #category : #private }
GLMBrickLayoutTraverser >> hasMaxDimensionRestriction: aBrick [

	^ self subclassResponsibility
]

{ #category : #private }
GLMBrickLayoutTraverser >> hasMinDimensionRestriction: aBrick [

	^ self subclassResponsibility
]

{ #category : #private }
GLMBrickLayoutTraverser >> isClean: aBrick [

	^ self subclassResponsibility
]

{ #category : #private }
GLMBrickLayoutTraverser >> isDependsOnChildren: aBrick [

	^ (self stateOf: aBrick) isOnChildren
]

{ #category : #private }
GLMBrickLayoutTraverser >> isDependsOnMyself: aBrick [

	^ (self stateOf: aBrick) isOnMyself
]

{ #category : #private }
GLMBrickLayoutTraverser >> isDependsOnParent: aBrick [

	^ (self stateOf: aBrick) isOnParent
]

{ #category : #private }
GLMBrickLayoutTraverser >> isLayoutClean: aBrick [

	^ aBrick
		ifNotNil: [ :b | b layout isClean ]
		ifNil: [ true ]
]

{ #category : #private }
GLMBrickLayoutTraverser >> isLayoutDirty: aBrick [

	^ aBrick
		ifNotNil: [ :b | b layout isDirty ]
		ifNil: [ true ]
]

{ #category : #private }
GLMBrickLayoutTraverser >> isOppositeClean: aBrick [

	^ self subclassResponsibility
]

{ #category : #traversing }
GLMBrickLayoutTraverser >> layoutAround: aBrick inContext: aContextBrick [
	"want to layout non-Brick object? nice try, bro."
	aBrick isBrick ifFalse: [ ^ self ].
	"if my dimension is clean why do you want to layout me?"
	(self isClean: aBrick) ifTrue: [ ^ self ].
	aBrick changed.
	"custom dependencies are very dangerous, as it's complicated
	to detect cycles fast"
	(self customDependencies: aBrick)
		do: [ :each | |b|
			[ b :=  (each brickValue: aBrick) brickApi ] on: Exception do: [ b := nil ] .
			b ifNotNil: [ self layoutAround: b inContext: aBrick ] ].
		
	(self stateOf: aBrick) layout: aBrick on: self.
	
	"and finally we position them"
	self layoutSubbricks: aBrick.
	self layoutSubbricks: aBrick owner.
	
	aBrick changed.

]

{ #category : #traversing }
GLMBrickLayoutTraverser >> layoutChildrenOf: aBrick [

	(self prioritySortedSubbricks: aBrick) do: [ :each | self layoutAround: each inContext: aBrick ]
]

{ #category : #traversing }
GLMBrickLayoutTraverser >> layoutOwnerOf: aBrick [

	aBrick owner ifNotNil: [ self layoutAround: aBrick owner inContext: aBrick ]
]

{ #category : #private }
GLMBrickLayoutTraverser >> layoutSubbricks: aBrick [
	aBrick ifNil: [ ^ self ].
	aBrick isBrick ifFalse: [ ^ self ].
	((self isClean: aBrick) & (self isOppositeClean: aBrick))
		ifFalse: [ ^ self ].
	(self isLayoutClean: aBrick) ifTrue: [ ^ self ].
	(aBrick subbricks allSatisfy: [ :each | each isBrick ifTrue: [ (self isClean: each) & (self isOppositeClean: each) ] ifFalse: [ false ] ])
		ifFalse: [ ^ self ].
	
	aBrick invalidateLayout.
	self markLayoutClean: aBrick.
	aBrick onChildrenLayouted
]

{ #category : #private }
GLMBrickLayoutTraverser >> markClean: aBrick [

	^ self subclassResponsibility
]

{ #category : #private }
GLMBrickLayoutTraverser >> markDirty: aBrick [

	^ self subclassResponsibility
]

{ #category : #traversing }
GLMBrickLayoutTraverser >> markDirtyAround: aBrick inContext: aContextBrick [
	aBrick isBrick ifFalse: [ ^ self ].
	aBrick = aContextBrick ifTrue: [ ^ self ].
	
	aBrick owner ifNotNil: [
		(aBrick owner = aContextBrick) not & aBrick owner isBrick ifTrue: [
			(self isClean: aBrick owner) ifTrue: [
				(self isDependsOnChildren: aBrick owner) ifTrue: [
					self markDirty: aBrick owner.
					self markDirtyAround: aBrick owner inContext: aBrick ].
			
				((self isDependsOnMyself: aBrick) | (self isDependsOnChildren: aBrick))
					ifTrue: [ (aBrick owner subbricks anySatisfy: [ :each | self isDependsOnParent: each]) ifTrue: [
						self markDirty: aBrick owner.
						self markDirtyChildrenAround: aBrick owner inContext: aBrick ] ].
				
				(self isClean: aBrick owner) ifTrue: [
					(aBrick layout floating = #none) not | (aBrick layout vAlign = #none) not | (aBrick layout hAlign = #none) not ifTrue: [ 
						self markLayoutDirty: aBrick owner ] ] ] ] ].
	
	aBrick subbricks do: [ :each |
		each = aContextBrick ifFalse: [
			each isBrick ifTrue: [
			((self isDependsOnParent: each) & (self isClean: each))
					| (each layout floating = #none) not | (each layout vAlign = #none) not | (each layout hAlign = #none) not ifTrue: [
				self markDirty: each.
				self markDirtyAround: each inContext: aBrick ] ] ] ].
	
	self markDirtyCustomDependency: aBrick inContext: aContextBrick.
	aBrick subbricks do: [ :each | self markDirtyCustomDependency: each inContext: aBrick ]
	

]

{ #category : #traversing }
GLMBrickLayoutTraverser >> markDirtyChildrenAround: aBrick inContext: aContextBrick [

	aBrick subbricks do: [ :each |
		each = aContextBrick ifFalse: [
			each isBrick ifTrue: [
			(self isClean: each) ifTrue: [
				self markDirty: each.
				self markDirtyAround: each inContext: aBrick ] ] ] ].
]

{ #category : #traversing }
GLMBrickLayoutTraverser >> markDirtyCustomDependency: aBrick inContext: aContextBrick [
	(aBrick isBrick) not | (aBrick = aContextBrick) ifTrue: [ ^ self ].
	
	(self hasCustomDependencies: aBrick) ifTrue: [
				self markDirty: aBrick.
				aBrick subbricks do: [ :each | each isBrick ifTrue: [
						self markDirty: each.
						self markDirtyAround: each inContext: aBrick ] ].
				
				(self customDependencies: aBrick) do: [ :each | |brick|
					[ brick := (each brickValue: aBrick) brickApi ] on: Exception do: [ brick := nil ].
					brick ifNotNil: [
						(brick = aContextBrick) not & (brick = aBrick) not & brick isBrick ifTrue: [
							(self isClean: brick) ifTrue: [
								self markDirty: brick.
								self markDirtyAround: brick inContext: aBrick ] ] ] ] ]
]

{ #category : #private }
GLMBrickLayoutTraverser >> markLayoutClean: aBrick [

	aBrick ifNotNil: [ :b | b markLayoutClean ]
]

{ #category : #private }
GLMBrickLayoutTraverser >> markLayoutDirty: aBrick [

	aBrick ifNotNil: [ :b | b isBrick ifTrue: [ b markLayoutDirty ] ]
]

{ #category : #accessing }
GLMBrickLayoutTraverser >> opposite [

	^ self subclassResponsibility
]

{ #category : #private }
GLMBrickLayoutTraverser >> prioritySortedSubbricks: aBrick [
	|sorted|
	sorted := OrderedCollection newFrom: aBrick subbricks.
	sorted sort: [ :first :second | (self isDependsOnParent: first) ifFalse: [ true ]
		 ifTrue: [ (self isDependsOnParent: second) ifFalse: [ false ]
			ifTrue: [ (self hasDimensionRestrictions: first) ifFalse: [ false ]
				ifTrue: [ (self hasDimensionRestrictions: second) ifFalse: [ true ]
					ifTrue: [ (self hasMinDimensionRestriction: first) ifFalse: [ false ]
						ifTrue: [ true ] ] ] ] ] ].
	^ sorted
]

{ #category : #accessing }
GLMBrickLayoutTraverser >> state [

	^ self subclassResponsibility
]

{ #category : #accessing }
GLMBrickLayoutTraverser >> stateOf: aBrick [

	^ self state cull: aBrick
]

{ #category : #private }
GLMBrickLayoutTraverser >> updateDimension: aBrick [

	^ self subclassResponsibility
]
