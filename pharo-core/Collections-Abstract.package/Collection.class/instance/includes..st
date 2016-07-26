testing
includes: anObject 
	"Answer whether anObject is one of the receiver's elements."

	^ self anySatisfy: [:each | each = anObject]