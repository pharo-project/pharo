enumerating
gatherChangeSets		"ChangeSet gatherChangeSets"
	"Collect any change sets created in other projects"
	| allChangeSets |
	allChangeSets := AllChangeSets asSet.
	self allSubInstances do: [:each | | obsolete |
		(allChangeSets includes: each) == (obsolete := each isMoribund) ifTrue:[
			obsolete
				ifTrue: ["Was included and is obsolete."
						AllChangeSets remove: each]
				ifFalse: ["Was not included and is not obsolete."
						AllChangeSets add: each]]].
	^ AllChangeSets