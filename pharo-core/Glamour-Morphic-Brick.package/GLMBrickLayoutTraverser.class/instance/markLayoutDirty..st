private
markLayoutDirty: aBrick

	aBrick ifNotNil: [ :b | b isBrick ifTrue: [ b markLayoutDirty ] ]