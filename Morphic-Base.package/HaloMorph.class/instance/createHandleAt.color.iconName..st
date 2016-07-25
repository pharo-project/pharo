private
createHandleAt: aPoint color: aColor iconName: iconName 
	| bou handle |
	bou := Rectangle center: aPoint extent: self handleSize asPoint.
	self gradientHalo
		ifTrue: [
			handle := Morph newBounds: bou color: aColor.
			handle borderWidth: 1.
			handle useRoundedCorners.
			self setColor: aColor toHandle: handle]
		ifFalse: [handle := EllipseMorph newBounds: bou color: aColor].
	""
	handle borderColor: aColor muchDarker.
	handle wantsYellowButtonMenu: false.
	""
	iconName isNil
		ifFalse: [| form | 
			form := Smalltalk ui icons iconNamed: iconName ifNone: [].
			form isNil
				ifFalse: [| image | 
					image := ImageMorph new.
					image form: form.
					image color: aColor contrastingForegroundColor.
					image lock.
					handle addMorphCentered: image]].
	""
	^ handle