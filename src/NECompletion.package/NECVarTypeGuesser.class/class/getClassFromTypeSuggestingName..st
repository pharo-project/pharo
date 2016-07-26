private
getClassFromTypeSuggestingName: aString 
	| firstUppercaseLetter className aStream aClass |
	aStream := WriteStream on: String new.
	aStream nextPut: aString first asUppercase.
	aStream
		nextPutAll: (aString copyFrom: 2 to: aString size).
	className := aStream contents.
	aClass := Smalltalk
				at: className asSymbol
				ifAbsent: [].
	(aClass isKindOf: Class)
		ifTrue: [^ aClass].
	firstUppercaseLetter := aString indexOfFirstUppercaseCharacter.
	className := firstUppercaseLetter > 1
				ifTrue: [aString copyFrom: firstUppercaseLetter to: aString size]
				ifFalse: [^ nil].
	aClass := Smalltalk
				at: className asSymbol
				ifAbsent: [].
	(aClass isKindOf: Class)
		ifTrue: [^ aClass].
	^ nil