private
needsReset

	^ currentSession ~~ Smalltalk session
		or: [ currentImagePathString ~= self imagePathString
		or: [ currentStore entriesCount >= self maximumNumberOfEntries ] ]