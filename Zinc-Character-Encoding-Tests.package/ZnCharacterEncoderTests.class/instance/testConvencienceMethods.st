testing
testConvencienceMethods
	| encoder string |
	encoder := ZnUTF8Encoder new.
	string := 'élève en Français'.
	self assert: (encoder decodeBytes: (encoder encodeString: string)) equals: string.
	self assert: (encoder encodedByteCountForString: string) = 20.
	
	#( 'ccc' 'ççç' 'c' 'ç' 'çc' 'cç' ) do: [ :each |
		self assert: (encoder decodeBytes: (encoder encodeString: each)) equals: each ]