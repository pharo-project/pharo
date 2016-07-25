translate
translate: aString
	"Dispatch to a registered translator to translate the given string or (if there is no translator) return the untranslated string."
	Current ifNotNil: [ ^Current translate: aString ].
	^ aString