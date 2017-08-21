accessing method dictionary
allSelectorsInProtocol: aName 
	"Answer a list of all the methods of the receiver and all its 
	superclasses that are in the protocol named aName"
	
	| aColl |
	aColl := OrderedCollection new.
	self withAllSuperclasses do: [:aClass | 
		aColl addAll: (aClass organization listAtCategoryNamed: aName) ].
	^ aColl asSet asArray sort