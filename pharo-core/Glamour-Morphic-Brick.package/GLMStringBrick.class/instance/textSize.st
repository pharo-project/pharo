accessing
textSize

	^ textSize ifNil: [ textSize := self sizeOf: self text ]