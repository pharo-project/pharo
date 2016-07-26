instance creation
fromList: anArray 
	"Construct a menu from an array of descriptions. See #addList comment"

	^ self new addList: anArray.
