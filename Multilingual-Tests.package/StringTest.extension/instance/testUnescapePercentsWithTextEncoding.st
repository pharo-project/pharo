*Multilingual-Tests-OtherLanguages
testUnescapePercentsWithTextEncoding
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