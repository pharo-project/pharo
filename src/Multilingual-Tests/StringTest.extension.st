Extension { #name : #StringTest }

{ #category : #'*Multilingual-Tests-OtherLanguages' }
StringTest >> testUnescapePercents [
	self assert: '' urlDecoded equals: ''.
	self assert: 'x' urlDecoded equals: 'x'.

	self assert: '+' urlDecoded equals: ' '.
	self assert: 'x+' urlDecoded equals: 'x '.
	self assert: '+x' urlDecoded equals: ' x'.
	self assert: 'x+x' urlDecoded equals: 'x x'.

	self should: [ '%' urlDecoded ] raise: Error.
	self should: [ '%3' urlDecoded ] raise: Error.
	self assert: '%3C' urlDecoded equals: '<'.

	self assert: '%3Cx%3E4%3C%2Fx%3E' urlDecoded equals: '<x>4</x>'.

	self assert: '!@#$%25%5E&*()%7B%7D%5B%5D=:/;?+''%22' urlDecoded equals: '!@#$%^&*(){}[]=:/;? ''"'.
	self assert: '!%40%23%24%25%5E%26*()%7B%7D%5B%5D%3D%3A%2F%3B%3F%2B''%22' urlDecoded equals: '!@#$%^&*(){}[]=:/;?+''"'.
	self assert: '%21@%23%24%25%5E%26*%28%29%7B%7D%5B%5D%3D%3A/%3B%3F+%27%22' urlDecoded equals: '!@#$%^&*(){}[]=:/;? ''"'
]

{ #category : #'*Multilingual-Tests-OtherLanguages' }
StringTest >> testUnescapePercentsWithTextEncoding [
	| leading kataTe kataSu kataTo |
	leading := JapaneseEnvironment leadingChar.
	"Katakana letter Te"
	kataTe := (Character leadingChar: leading code: 12486) asString.
	"Katakana letter Su"
	kataSu := (Character leadingChar: leading code: 12473) asString.
	"Katakana letter To"
	kataTo := (Character leadingChar: leading code: 12488) asString.
	"This could be reactivated when/if Zinc-Character-Encoding supports shift_jis"
	"self assert: ('%83e%83X%83g' unescapePercentsWithTextEncoding: 'shift_jis')
			= (kataTe , kataSu , kataTo).
	self assert: ('%83e%83X%83g%20and%20%83e%83X%83g' unescapePercentsWithTextEncoding: 'shift_jis')
			= (kataTe , kataSu , kataTo , ' and ' , kataTe , kataSu , kataTo)"
]
