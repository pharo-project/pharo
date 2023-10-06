Extension { #name : 'ShiftClassBuilder' }

{ #category : '*Deprecated12' }
ShiftClassBuilder >> trait: aTraitComposition [

	self deprecated: 'Please use #traits: instead' transformWith: '`@receiver trait: `@arg' -> '`@receiver traits: `@arg'.

	^ self traits: aTraitComposition
]

{ #category : '*Deprecated12' }
ShiftClassBuilder >> uses: aTraitComposition [

	self deprecated: 'Please use #traits: instead' transformWith: '`@receiver trait: `@arg' -> '`@receiver traits: `@arg'.

	^ self traits: aTraitComposition
]
