testing
testUTF8ReadFaultyInput
	"Although not recommended, it is possible to read faulty UTF-8 encoded input by resuming ZnInvalidUTF8"
	
	"illegal leading byte"
	self 
		should: [ #[102 111 111 160 102 111 111] utf8Decoded ] 
		raise: ZnInvalidUTF8.
	self 
		assert: ([ #[102 111 111 160 102 111 111] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume ]) 
		equals: 'foo?foo'.
	self 
		assert: ([ #[102 111 111 160 102 111 111] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume: $_ codePoint ]) 
		equals: 'foo_foo'.
	
	"illegal continuation byte"
	self 
		should: [ #[102 111 111 195 0 102 111 111] utf8Decoded ] 
		raise: ZnInvalidUTF8.
	self 
		assert: ([ #[102 111 111 195 0 102 111 111] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume ]) 
		equals: 'foo?foo'.

	"incomplete input"
	self 
		should: [ #[102 111 111 195 ] utf8Decoded ] 
		raise: ZnInvalidUTF8.
	self 
		assert: ([ #[102 111 111 195 ] utf8Decoded ] on: ZnInvalidUTF8 do: [ :exception | exception resume ]) 
		equals: 'foo?'