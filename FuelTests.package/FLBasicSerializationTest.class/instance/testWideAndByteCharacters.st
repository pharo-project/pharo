tests
testWideAndByteCharacters
    "Since wide and byte characters are represented in the same class, this case is prone to fail."
	
	| materialized aByteCharacter aWideCharacter|
	aWideCharacter := Character value: 12345.
	aByteCharacter := $a.
	materialized := self resultOfSerializeAndMaterialize: (Array with: aWideCharacter with: aByteCharacter ).
	self assert: materialized first = aWideCharacter.
	self assert: materialized second == aByteCharacter.