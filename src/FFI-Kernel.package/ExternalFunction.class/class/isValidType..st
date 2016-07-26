compiler support
isValidType: anObject
	^anObject isBehavior and:[anObject includesBehavior: ExternalStructure]