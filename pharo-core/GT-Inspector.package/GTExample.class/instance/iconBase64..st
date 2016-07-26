pragma-selectors
iconBase64: aString
	<gtExamplePragma>
	<description: 'Attach an icon to this example. The string must represent a base64 string'>
	
	self initializeIconWith: [ 
		form := (Form fromBinaryStream: (Base64MimeConverter mimeDecodeToBytes: aString readStream)) ]