private
basicHandle: aHandle at: index
	^ self objectClass fromInteger: (aHandle 
		perform: self externalType atomicSelector 
		with: index)