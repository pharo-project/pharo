*System-Support
allCallsOnIn: aSystemNavigation
	"Answer a SortedCollection of all the methods that refer to me by name or as part of an association in a global dict."

	^ (aSystemNavigation 
		allReferencesTo: (self environment associationAt: self theNonMetaClass name)), 
		(aSystemNavigation allCallsOn: self theNonMetaClass name)	