loading
loadPatch
	| old new |
	(self zip memberNamed: 'patch.bin') ifNotNil:
		[:m | [^ patch := (MCDataStream on: m contentStream) next ]
			on: Error do: [:fallThrough |]].
	definitions := OrderedCollection new.
	(self zip membersMatching: 'old/*')
		do: [:m | self extractDefinitionsFrom: m].
	old := definitions asArray.
	definitions := OrderedCollection new.
	(self zip membersMatching: 'new/*')
		do: [:m | self extractDefinitionsFrom: m].
	new := definitions asArray.
	^ patch := self buildPatchFrom: old to: new.
	