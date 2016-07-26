private
selectedItemsProbablyRestricted

	| matcher string |
	string := self selectedItemsSearchingString.
	matcher := string
					ifEmpty: ['*']
					ifNotEmpty: [
						(string last = $*) 
							ifTrue: [string]
							ifFalse:[string, '*']].
	^self selectedItems select: [:each | matcher match: each].