traversing
markDirtyCustomDependency: aBrick inContext: aContextBrick
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