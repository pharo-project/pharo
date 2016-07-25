properties
propertyValueAt: aKey
	"Answer the property value associated with aKey."
	
	^ self propertyValueAt: aKey ifAbsent: [ self error: 'Property not found' ].