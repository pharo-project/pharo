accessing
widthOrUndefined
	^ self width ifNil: [ self class undefinedColumnWidth ]