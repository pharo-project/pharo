instance creation
openContents: aString label: aLabel
	"Open a new playground with the given contents"

	| page |

	page := GTPlayPage new
		saveContent: aString;
		title: aLabel;
		yourself.
	^ self openOn: page