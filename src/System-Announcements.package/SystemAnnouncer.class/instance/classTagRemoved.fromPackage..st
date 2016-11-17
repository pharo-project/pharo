triggering
classTagRemoved: tagName fromPackage: aPackage

	self announce: (ClassTagRemoved in: aPackage tag: tagName)