private factory
createFFICallout
	^ FFICallout new
		sender: self sender;
		options: self calloutAPI options;
		yourself