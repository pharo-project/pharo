open/close
open: fileName forWrite: writeMode 
	| result |
	result := super open: fileName forWrite: writeMode.
	result ifNotNil: [
			converter ifNil: [self converter: UTF8TextConverter new].
			lineEndConvention ifNil: [ self detectLineEndConvention ]
	].
	^result