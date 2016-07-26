accessing
selectionInterval
	
	^ self valueOfProperty: #phlowSelectionInterval ifAbsent: [ 1 to: 0 ]