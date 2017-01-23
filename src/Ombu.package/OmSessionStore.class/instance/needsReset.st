private
needsReset

	^ currentSession ~~ Smalltalk session
		or: [ currentImagePathString ~= self imagePathString ]