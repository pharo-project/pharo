moving changes
forgetChangesForClass: className in: otherChangeSet
	"See forgetAllChangesFoundIn:.  Used in culling changeSets."

	(self changeRecorderFor: className)
			forgetChangesIn: (otherChangeSet changeRecorderFor: className).
