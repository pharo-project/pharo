accessing
languageIndex

	| item |
	item := self pangrams detect: [ :one | one value = selectedPangram ] ifNone: nil.
	^self pangrams indexOf: item.