instance creation
new
	"All current changeSets must be registered in the AllChangeSets collection.
	Due to a quirk of history, this is maintained as class variable of ChangeSorter."

	^ self basicNewChangeSet: self defaultName