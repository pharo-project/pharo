moving changes
absorbClass: className from: otherChangeSet
	"Absorb into the receiver all the changes found in the class in the other change set.
	*** Classes renamed in otherChangeSet may have problems"

	(self changeRecorderFor: className) assimilateAllChangesIn: (otherChangeSet changeRecorderFor: className).
