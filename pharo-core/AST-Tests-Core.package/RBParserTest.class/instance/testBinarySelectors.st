tests parsing
testBinarySelectors
	"ANSI Smalltalk Standard Draft, page 32:
		binaryCharacter ::=
			'!' | '%' | '&' | '*' | '+' | ','' | '/' | '<' | '=' | '>' | '?' | '@' | '\' | '~' | '|' | '-'
		binarySelector ::= binaryCharacter+"

	| binaryCharacters selectorStrings |

	binaryCharacters := '!%&*+,/<=>?@\~-'.
	selectorStrings := OrderedCollection new.
	binaryCharacters do: [:first |
		selectorStrings addLast: (String with: first).
		binaryCharacters do: [:second |
			selectorStrings addLast: (String with: first with: second).
			binaryCharacters do: [:third |
				selectorStrings addLast: (String with: first with: second with: third)]]].

	selectorStrings do: [:each | | methodNode messageNode |
		methodNode := RBParser parseMethod: each, 'anObject
	^self',each,'anObject'.
		self assert: methodNode selector = each asSymbol.
		self assert: methodNode argumentNames asArray = #(anObject).
		self assert: methodNode statements size = 1.
	
		messageNode := methodNode statements first value.
		self assert: messageNode isMessage.
		self assert: messageNode receiver name = #self.
		self assert: messageNode selector = each asSymbol.
		self assert: messageNode arguments size = 1.
		self assert: messageNode arguments first isValue.
		self assert: messageNode arguments first name = #anObject].