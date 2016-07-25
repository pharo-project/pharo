private
unselectedItemsProbablyRestricted

	| matcher string |
	string := self unselectedItemsSearchingString.
	matcher := string
					ifEmpty: ['*']
					ifNotEmpty: [
						(string last = $*) 
							ifTrue: [string]
							ifFalse:[string, '*']].
	^self unselectedItems select: [:each | matcher match: each].