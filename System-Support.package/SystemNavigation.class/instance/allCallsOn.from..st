query
allCallsOn: aSymbol from: aClass
	"Answer a SortedCollection of all the methods that call on aSymbol."

	| collection |
	collection := OrderedCollection  new.
	aClass withAllSubclassesDo: [ :class |
		(class whichSelectorsReferTo: aSymbol) do: [:sel |
				collection add: (self createMethodNamed: sel realParent: class)]].
	^collection