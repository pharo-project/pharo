Extension { #name : 'OCAssignmentNode' }

{ #category : '*Reflectivity-Examples' }
OCAssignmentNode >> tagType: anObject [
	"record the type of the assigned value"

	self variable propertyAt: #types ifAbsentPut: Set new.
	(self variable propertyAt: #types) add: anObject class
]
