initialize-release
escape: char with: anObject
	"Instruct me to escape char with object, either a replacement string or #pass"
	
	"self escape: $/ with: '\/'."
	
	self assert: (anObject isString | (anObject == #pass)).
	self assert: char isOctetCharacter.
	self writeableStonCharacters at: char codePoint + 1 put: anObject