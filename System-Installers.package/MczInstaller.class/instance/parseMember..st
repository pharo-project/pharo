utilities
parseMember: memberOrName
	| member tokens |
	member := self zip member: memberOrName.
	tokens := (self contentsForMember: member) parseLiterals first.
	^ self associate: tokens