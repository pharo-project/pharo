accessing
traitUsers
	"Retrieves the receiver's classes and traits"
	
	^users ifNil:[ users := OrderedCollection new ]