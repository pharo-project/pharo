private
basicHandle: aHandle at: index
	^ self objectClass fromHandle: (aHandle pointerAt: index)