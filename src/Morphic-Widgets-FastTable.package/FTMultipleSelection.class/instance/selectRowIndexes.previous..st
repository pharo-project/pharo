private
selectRowIndexes: rowIndex previous: oldSelection		
	"I ensure the selected row index is the first in selection range so I can handle it better. 
	 I do not like to assume and probably I will need the concept of 'selection', but for now 
	 let's not abuse :)"
	self table selectRowIndexes: (((oldSelection includes: rowIndex)
		ifTrue: [ oldSelection copyWithout: rowIndex ]
		ifFalse: [ oldSelection] )
		copyWithFirst: rowIndex)