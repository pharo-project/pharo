creating
createProperty: aKey with: aValue
	"Create an empty attribute value pair. Subclasses might want to refine this implementation."
	
	^ aKey -> aValue