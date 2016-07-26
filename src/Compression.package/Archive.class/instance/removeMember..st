archive operations
removeMember: aMemberOrName
	| member |
	member := self member: aMemberOrName.
	member ifNotNil: [ members remove: member ].
	^member