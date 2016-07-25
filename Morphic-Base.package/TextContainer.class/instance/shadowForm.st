private
shadowForm
	shadowForm ifNil: [self computeShadow].
	^ shadowForm