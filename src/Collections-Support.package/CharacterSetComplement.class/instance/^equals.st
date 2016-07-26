comparing
= anObject
	"Implementation note: we do not test if equal to a WideCharacterSet,
	because it is unlikely that WideCharacterSet is as complete as self"
	
	^self class == anObject class and: [
		absent = anObject complement ]