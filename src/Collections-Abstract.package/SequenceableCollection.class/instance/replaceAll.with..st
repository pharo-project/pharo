accessing
replaceAll: oldObject with: newObject 
	"Replace all occurences of oldObject with newObject"
	| index |
	index := self
				indexOf: oldObject
				startingAt: 1
				ifAbsent: [0].
	[index = 0]
		whileFalse: 
			[self at: index put: newObject.
			index := self
						indexOf: oldObject
						startingAt: index + 1
						ifAbsent: [0]]