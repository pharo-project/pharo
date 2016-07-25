private
selectRowIndexesUpTo: endIndex
	"Selects a range of rows, from what is already selected to a new rowIndex. 
	 This is used in case of multiple selections (when holding shift)"
	| oldSelected firstIndex step |

	oldSelected := self table selectedRowIndexes.
	firstIndex :=  oldSelected 
		ifNotEmpty: [ oldSelected first ]
		ifEmpty: [ endIndex ].
	step := firstIndex <= endIndex 
		ifTrue: [ 1 ] 
		ifFalse: [ -1 ]. 

	self 
		selectRowIndexes: endIndex 
		previous: (oldSelected union: (firstIndex to: endIndex by: step))