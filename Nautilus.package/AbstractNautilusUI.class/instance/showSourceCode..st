source code area
showSourceCode: anAnnouncement
	| formatter |
	formatter := RBProgramNode formatterClass.
	self sourceTextModel setText: self sourceCode.
	formatter formatAsYouReadPolicy
		ifTrue: [ 
			self sourceTextModel
				formatSourceCodeInView].
	self sourceTextModel clearUndoManager.
	self sourceTextModel hasUnacceptedEdits: false