own services
basicDeleteDirectory
	"Remove the currently selected directory"
	reference entries isEmpty
		 ifFalse:[^self inform:'Directory must be empty'].
	
	(self confirm: 'Really delete ' , reference basename , '?') 
		ifFalse: [^ self].
	
	self volumeListIndex: self volumeListIndex-1.

	reference delete.
	self directory: reference parent