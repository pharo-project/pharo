accessing
getCurrentSelectionIndex
	"Answer the index of the current selection."
	self getIndexSelector ifNil: [ ^0 ].
	^self model perform: self getIndexSelector