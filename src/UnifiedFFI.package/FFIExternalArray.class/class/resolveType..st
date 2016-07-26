private
resolveType: aType
	 ^ FFICallout new 
		requestor: self;
		resolveType: aType.