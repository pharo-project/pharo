*MonticelloGUI
openAndEditTemplateCopy
	^ self class fillInTheBlankConfigure: (self asCreationTemplate ifNil: [^nil])