accessing-computed
groups
	"Return the list of groups for the configuration"

	| groupSpecs |
	groups ~~ nil
		ifTrue: [ ^ groups ].
	groups := OrderedCollection new.
	self versionForInfoList ifNil: [ ^ groups ].
	groupSpecs := self versionForInfoList groups.
	groupSpecs := groupSpecs asSortedCollection: [ :a :b | a name <= b name ].
	groups
		addAll:
			(groupSpecs
				collect: [ :grpSpec | 
					(MBGroupInfo spec: grpSpec)
						configurationInfo: self configurationInfo;
						yourself ]).
	^ groups