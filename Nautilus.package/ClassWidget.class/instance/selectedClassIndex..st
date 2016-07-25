private list model
selectedClassIndex: anInteger
	| selection |
	
	selection := self getClassesList at: anInteger ifAbsent: [nil].
	
	self model showInstance ifFalse: [selection ifNotNil: [selection := selection theMetaClass]].
	self model selectedClassWithoutChangingSelection: selection.
	
	self changed: #selectedClassIndex.
	self model changed: #currentHistoryIndex.