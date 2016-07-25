instance creation
with: anObject 
	"Answer a new instance of me, containing only anObject."

	| newCollection |
	newCollection := self new: 1.
	newCollection at: 1 put: anObject.
	^newCollection