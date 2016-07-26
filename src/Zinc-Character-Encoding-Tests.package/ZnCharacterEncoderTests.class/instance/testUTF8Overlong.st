testing
testUTF8Overlong
	"Overlong encoding, aka Non shortest form characters should be rejected.
	See http://en.wikipedia.org/wiki/UTF-8#Overlong_encodings"
	
	self 
		should: [ ZnUTF8Encoder new decodeBytes: #[ 16rF0 16r82 16r82 16rAC ] ] 
		raise: ZnInvalidUTF8.
	self 
		should: [ ZnUTF8Encoder new decodeBytes: #(193 129 193 130 193 131 ) ] 
		raise: ZnInvalidUTF8.
		
	