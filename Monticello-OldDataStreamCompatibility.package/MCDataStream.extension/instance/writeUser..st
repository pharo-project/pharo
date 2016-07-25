*Monticello-OldDataStreamCompatibility
writeUser: anObject
    "Write the contents of an arbitrary User instance (and its devoted class)."

	"If anObject is an instance of a unique user class, will lie and say it has a generic class"
    ^ anObject storeDataOn: self