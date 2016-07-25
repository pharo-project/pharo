tests
testStrings
	self assert: (STONJSON toString: 'string') equals: '"string"'.
	self assert: (STONJSON toString: '') equals: '""'.
	self assert: (STONJSON toString: 'élèves français') equals: '"élèves français"'.
	self assert: (STONJSON toString: String crlf) equals: '"\r\n"'.
	self assert: (STONJSON fromString: '"string"') equals: 'string'.
	self assert: (STONJSON fromString: '""') equals: ''.
	self assert: (STONJSON fromString: '"élèves français"') equals: 'élèves français'.
	self 
		assert: (STONJSON fromString: '"\u212B \"Angstrom Symbol\""') 
		equals: 8491 asCharacter asString, ' "Angstrom Symbol"'.