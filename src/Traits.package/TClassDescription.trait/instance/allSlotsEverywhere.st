slots
allSlotsEverywhere
	"Answer the set of slots used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList := OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls slots].
	^ aList asSet