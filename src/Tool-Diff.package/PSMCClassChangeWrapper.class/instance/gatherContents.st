accessing
gatherContents
	"Answer the contents of the change from the model."

	^((self model select: [:i | i targetClassName = self item and: [
			i definition isClassDefinition not]])
		collect: [:o |  o patchWrapper model: self model]) asSortedCollection: [:a :b |
			a asString <= b asString]