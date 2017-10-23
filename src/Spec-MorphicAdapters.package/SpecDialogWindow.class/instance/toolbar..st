accessing
toolbar: anObject

	toolbar := anObject.
	anObject owner: self.
	
	self removeMorph: self submorphs last.
	self addMainPanel