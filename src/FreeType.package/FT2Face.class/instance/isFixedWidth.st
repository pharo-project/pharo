testing
isFixedWidth
	styleFlags == nil ifTrue:[^false].
	^faceFlags allMask: 4 "FT_FACE_FLAG_FIXED_WIDTH" 