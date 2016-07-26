testing
testUTF8ReadStreamReadInto
	| string bytes stream buffer |
	string := 'élève en Français'.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterReadStream on: bytes readStream.
	buffer := String new: string size.
	stream next: string size into: buffer. 
	self assert: buffer equals: string.
	self assert: stream atEnd.
	string := 'Czech in Czech is {1}e{2}tina.' format: { 269 asCharacter. 353 asCharacter }.
	bytes := ZnUTF8Encoder new encodeString: string.
	stream := ZnCharacterReadStream on: bytes readStream.
	buffer := String new: string size.
	stream next: string size into: buffer. 
	self assert: buffer equals: string.
	self assert: stream atEnd	