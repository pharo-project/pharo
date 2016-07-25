private
globals: aSystemDictionary
	"Sets the system-wide globals"
	
	globals ifNotNil: [self error: 'Cannot overwrite existing globals'].
	globals := aSystemDictionary