stack list
browseContext
	selectedContext ifNil: [ ^ self ]. 
	Smalltalk tools browser openOnClass: self selectedClass selector: self selectedSelector