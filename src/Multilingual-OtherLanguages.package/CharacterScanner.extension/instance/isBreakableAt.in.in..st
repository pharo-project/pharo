*Multilingual-OtherLanguages
isBreakableAt: index in: sourceString in: encodingClass
"check with the encoding whether the character at index is a breakable character.
Only the JISX0208 & JapaneseEnvironments  ever return true, so only the scanJapaneseCharacters... method calls this"
	^ encodingClass isBreakableAt: index in: sourceString.
