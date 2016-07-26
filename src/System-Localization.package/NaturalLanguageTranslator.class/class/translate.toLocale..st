translate
translate: aString toLocale: localeID
	"Dispatch to a registered translator to translate the given string with the given locale ID or (if there is no translator) return the untranslated string."
	Current ifNotNil: [ ^Current translate: aString toLocale: localeID ].
	^ aString