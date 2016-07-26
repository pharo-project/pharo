class initialization
initializeClassificationTable
	PatternVariableCharacter := $`.
	KeywordPatternCharacter := $@.
	CascadePatternCharacter := $;.
	classificationTable := Array new: 255.
	self 
		initializeChars: (Character allCharacters
			select: [ :each | each isLetter ])
		to: #alphabetic.
	self initializeUnderscore.
	self initializeChars: '01234567890' to: #digit.
	self initializeChars: '!%&*+,-/<=>?@\~|' to: #binary.
	classificationTable at: 177 put: #binary.	"plus-or-minus"
	classificationTable at: 183 put: #binary.	"centered dot"
	classificationTable at: 215 put: #binary.	"times"
	classificationTable at: 247 put: #binary.	"divide"
	self initializeChars: '().:;[]{}^' to: #special.
	self
		initializeChars: (Character allCharacters
			select: [ :each | each isSeparator ])
		to: #separator