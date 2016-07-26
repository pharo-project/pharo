private
basicHandle: aHandle at: index put: value
	^ aHandle 
		perform: (self externalType atomicSelector,#put:) asSymbol
		with: index
		with: value getHandle