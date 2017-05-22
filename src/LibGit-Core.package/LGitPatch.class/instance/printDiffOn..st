operations
printDiffOn: aStream
	| callback |
	callback := LGitDiffLineCallback on: [ :delta :hunk :line :payload |
		aStream nextPutAll: line content. 0 ].
	
	self withReturnHandlerDo: [
		self
			patch_print: self
			print_cb: callback
			payload: nil ]