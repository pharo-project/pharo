scripting opening
openOn: anObject
	
	^ (super openOn: anObject)
		extent: self initialExtent;
		title: self session name;
		yourself