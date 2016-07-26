private
addSubclassesOf: aClass in: result withIndex: index fromRestrictedCollection: aCollection

	| classes |

	classes := aClass subclasses" intersection: self shownClasses".
	
	classes := classes intersection: aCollection.
	(classes sort: [:a :b| a name <= b name]) do: [:each |
		result at: each put: index.
		self addSubclassesOf: each in: result withIndex: index + 1 fromRestrictedCollection: aCollection ]