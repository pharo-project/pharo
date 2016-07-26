*metacello-mc
asRepositorySpecFor: aMetacelloMCProject

	| desc |
	desc := self description.
	desc ifNil: [ desc := 'dictionary://Metacello_Dictionary' ].
	^(aMetacelloMCProject repositorySpec)
		description:  desc;
	 	type: 'dictionary';
		yourself