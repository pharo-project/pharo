pointing to
pointersToExcept: objectsToExclude
	"Find all objects in the system that hold a pointer to me, excluding those listed"
	| c pointers object objectsToAlwaysExclude |
	Smalltalk garbageCollect.
	pointers := OrderedCollection new.

	SystemNavigation default allObjectsDo: [ :e | (e pointsTo: self) ifTrue: [ pointers add: e ]  ].

	objectsToAlwaysExclude := {
		thisContext.
		thisContext sender.
		thisContext sender sender.
		objectsToExclude.
	}.
	c := thisContext.
	^(pointers removeAllSuchThat: [ :ea |
		(ea == thisContext sender) or: [ "warning: this expression is dependent on closure structure of this method"
			(objectsToAlwaysExclude identityIncludes: ea)
				or: [objectsToExclude identityIncludes: ea ]] ]) asArray