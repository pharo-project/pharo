dnd requests
requestDropStream: dropIndex
	"Request a read-only stream for some file that was dropped onto the application"
	^self concreteStream new requestDropStream: dropIndex.