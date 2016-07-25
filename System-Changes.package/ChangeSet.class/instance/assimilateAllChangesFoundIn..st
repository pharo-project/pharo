moving changes
assimilateAllChangesFoundIn: otherChangeSet
	"Make all changes in otherChangeSet take effect on self as if they happened just now."

	otherChangeSet changedClassNames do:
		[:className | self absorbClass: className from: otherChangeSet]
