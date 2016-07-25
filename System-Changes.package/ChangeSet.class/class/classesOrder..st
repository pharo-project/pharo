filein/out
classesOrder: classes
	"Answer a collection with the classes ordered so they can be filed in."

	| listInOrder |
	"SharedPools need to go first"
	listInOrder := Class superclassOrder: (classes select: [ :each | each inheritsFrom: SharedPool ]).
	listInOrder addAll: (Class superclassOrder: (classes reject: [ :each | each inheritsFrom: SharedPool ])).
	^ listInOrder