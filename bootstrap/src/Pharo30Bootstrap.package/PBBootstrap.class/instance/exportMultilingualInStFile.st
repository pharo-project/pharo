preparation
exportMultilingualInStFile

	self
		exportPackages: #( 'Multilingual-Languages' 'Multilingual-Encodings' 'Multilingual-TextConversion' )
		usingInitializeScript: '
		EncodedCharSet initialize.

		Unicode initializeTagConstants.
		UnicodeDefinitions initializeUnicodeClass.
		Character characterSet: Unicode.
		
		ByteTextConverter initialize.'
		intoFile: 'Multilingual.st'