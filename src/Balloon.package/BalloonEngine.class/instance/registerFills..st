drawing
registerFills: fills

	| fillIndexList index fillIndex |
	((colorTransform notNil and:[colorTransform isAlphaTransform]) or:[
		fills anySatisfy: [:any| any notNil and:[any isTranslucent]]])
			ifTrue:[	self flush.
					self reset.
					postFlushNeeded := true].
	fillIndexList := WordArray new: fills size.
	index := 1.
	[index <= fills size] whileTrue:[
		fillIndex := self registerFill: (fills at: index).
		fillIndex == nil 
			ifTrue:[index := 1] "Need to start over"
			ifFalse:[fillIndexList at: index put: fillIndex.
					index := index+1]
	].
	^fillIndexList