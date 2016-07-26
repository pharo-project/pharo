traversing
markDirtyAround: aBrick inContext: aContextBrick
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
	
