initialize-release
newFaceFromExternalMemory: aFreeTypeExternalMemory index: anInteger
	| memSize |

	aFreeTypeExternalMemory validate.
	memSize := aFreeTypeExternalMemory bytes size.
	self 
		primNewFaceFromExternalMemory: aFreeTypeExternalMemory 
		size: memSize 
		index: anInteger.
	
	self isValid
		ifTrue:[ self class register: self ]