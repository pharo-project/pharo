private
sanitize: aText
	| lineEndings |
	lineEndings := { Character cr. Character lf }.
	"Remove line endings that may have been introduced via copy-paste, but only if we have to because it will remove all text attributes"
	^ (aText includesAnyOf: lineEndings)
		ifTrue: [ aText copyWithoutAll: lineEndings ]
		ifFalse: [ aText ]