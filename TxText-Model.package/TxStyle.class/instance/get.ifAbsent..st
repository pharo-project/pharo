accessing
get: anAttributeClass ifAbsent: aBlock

	^ (attributes at: anAttributeClass ifAbsent: aBlock) value