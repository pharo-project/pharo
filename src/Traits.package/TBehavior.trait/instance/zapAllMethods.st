accessing method dictionary
zapAllMethods
	"Remove all methods in this class which is assumed to be obsolete"

	self methodDict: self emptyMethodDictionary.
	self class isMeta ifTrue: [self class zapAllMethods]