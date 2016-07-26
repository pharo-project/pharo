pragma-selectors
iconBytes: aByteArray
	<gtExamplePragma>
	<description: 'Attach an icon to this example. The icon form is described by the given byte-array'>
	
	self initializeIconWith: [ 
		form := (Form
			extent: 16@16
			depth: 32
			fromArray: aByteArray 
			offset: 0@0) ]