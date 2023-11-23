Extension { #name : 'ShiftClassBuilder' }

{ #category : '*Deprecated12' }
ShiftClassBuilder >> category [

	self deprecated: 'Categories are not supported anymore in Pharo. You can use #package and #tag instead to avoid any ambiguity on what part of the category is the package and what is the tag.'.
	^ self tag
		  ifNotNil: [ :packageTag | self package , '-' , packageTag ]
		  ifNil: [ self package ]
]

{ #category : '*Deprecated12' }
ShiftClassBuilder >> category: aString [

	| converter |
	self deprecated: 'Trying to package a class using the concept of categpry is not supported anymore in Pharo. You can use #package: and #tag: instead to avoid any ambiguity on what part of the category is the package and what is the tag.'.
	converter := CategoryConverter category: aString environment: self installingEnvironment.
	self package: converter packageName.
	self tag: converter tagName
]

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
