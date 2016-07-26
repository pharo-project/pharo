archive operations
contentsOf: aMemberOrName
	| member |
	member := self member: aMemberOrName.
	member ifNil: [ ^nil ].
	^member contents