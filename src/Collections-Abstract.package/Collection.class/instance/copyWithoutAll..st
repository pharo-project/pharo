copying
copyWithoutAll: aCollection
	"Answer a copy of the receiver that does not contain any elements 
	equal to those in aCollection."

	^ self reject: [:each | aCollection includes: each]