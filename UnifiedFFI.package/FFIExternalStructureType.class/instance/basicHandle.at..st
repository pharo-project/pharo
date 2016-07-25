private
basicHandle: aHandle at: index
	^ self objectClass fromHandle: (aHandle referenceStructAt: index length: self externalTypeSize)
