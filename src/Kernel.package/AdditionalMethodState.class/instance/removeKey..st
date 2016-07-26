properties
removeKey: aKey
	"Remove the property with aKey. Answer the property or raise an error if aKey isn't found."
	
	^ self removeKey: aKey ifAbsent: [ self error: 'Property not found' ].