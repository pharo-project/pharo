testing class hierarchy
includesBehavior: aClass
	^self == aClass or: [self inheritsFrom: aClass]