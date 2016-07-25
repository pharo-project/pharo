moving changes
forgetAllChangesFoundIn: otherChangeSet
	"Remove from the receiver all method changes found in aChangeSet. The intention is facilitate the process of factoring a large set of changes into disjoint change sets.  To use:  in a change sorter, copy over all the changes you want into some new change set, then use the subtract-other-side feature to subtract those changes from the larger change set, and continue in this manner."

	otherChangeSet == self ifTrue: [^ self].
	otherChangeSet changedClassNames do:
		[:className | self forgetChangesForClass: className in: otherChangeSet].
	self expungeEmptyClassChangeEntries.
