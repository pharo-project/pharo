instance variables
allInstVarNamesEverywhere
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList := OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BorderedMorph allInstVarNamesEverywhere"