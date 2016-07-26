private
doJoin
	scanner pc < blockEnd ifTrue:
		[stackPointer := joinOffsets at: scanner pc]