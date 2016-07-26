accessing
bbox
	bbox
		ifNil: [bbox := Rectangle new.
			self primLoadBbox: bbox].
	^ bbox