class tags
addClassTag: aSymbol
	"Add the class tag from the receiver, if already added do nothing."
	| tagName newTag |

	"strip package name if needed"
	tagName := self toTagName: aSymbol.
	
	^ self 
		classTagNamed: tagName
		ifAbsent: [ 
			self class organizer 
				validateCanBeAddedPackage: self
				tagName: tagName.
			newTag := self basicAddClassTag: tagName.
			SystemAnnouncer uniqueInstance classTagAdded: tagName inPackage: self.
			^newTag]