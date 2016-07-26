private
isLayoutDirty: aBrick

	^ aBrick
		ifNotNil: [ :b | b layout isDirty ]
		ifNil: [ true ]