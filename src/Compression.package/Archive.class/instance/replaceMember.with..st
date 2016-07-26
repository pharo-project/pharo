archive operations
replaceMember: aMemberOrName with: newMember
	| member |
	member := self member: aMemberOrName.
	member ifNotNil: [ members replaceAll: member with: newMember ].
	^member