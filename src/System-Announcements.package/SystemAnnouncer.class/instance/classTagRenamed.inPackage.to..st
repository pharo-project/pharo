triggering
classTagRenamed: tagName inPackage: aPackage to: newTagName

	self announce: (ClassTagRenamed in: aPackage tag: tagName to: newTagName)