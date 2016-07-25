accessing
get: anAttributeClass

	^ self get: anAttributeClass ifAbsent: [ anAttributeClass defaultValue ]