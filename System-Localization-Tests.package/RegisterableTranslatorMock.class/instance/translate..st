translate
translate: aString
	"Dispatch to a registered translator to translate the given string or (if there is no translator) return the untranslated string."
	^translations at: aString asSymbol ifAbsent: [ aString ] 
