copying
veryDeepCopy
	"Do a complete tree copy using a dictionary.  An object in the tree twice is only copied once.  All references to the object in the copy of the tree will point to the new copy."

	| copier new |
	copier := DeepCopier new initialize: 4096 "self initialDeepCopierSize".
	new := self veryDeepCopyWith: copier.
	copier references associationsDo: [:assoc | 
		assoc value veryDeepFixupWith: copier].
	copier fixDependents.
	^ new