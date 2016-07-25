query
allAccessesTo: instVarName from: aClass
	| coll |
	coll := OrderedCollection new.
	aClass
		withAllSubAndSuperclassesDo: [ :class | (class whichSelectorsAccess: instVarName) do: [ :sel | coll add: (self createMethodNamed: sel realParent: class) ] ].
	^ coll