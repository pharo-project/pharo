private
fieldSpec
	^ FFIExternalStructureFieldParser new  
		parseFields: self fieldsDesc structure: self.
