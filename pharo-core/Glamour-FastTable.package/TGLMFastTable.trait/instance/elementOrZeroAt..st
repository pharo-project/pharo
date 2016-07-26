private
elementOrZeroAt: rowIndex
	^ rowIndex = 0 
		ifTrue: [ 0 ] 
		ifFalse: [ self elementAt: rowIndex ] 
	