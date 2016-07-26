generate scripts/methods
getLatestUpdateNumber
	"self new getLatestUpdateNumber"
	
	| upfroms |
	upfroms := self class selectors select: [:each | 'update*' match: each ].
	upfroms := upfroms collect: [:each | [(each asString last: 5) asNumber] on: Error do: [0]].
	^ upfroms asSortedCollection last