private
isLayoutClean: aBrick

	^ aBrick
		ifNotNil: [ :b | b layout isClean ]
		ifNil: [ true ]