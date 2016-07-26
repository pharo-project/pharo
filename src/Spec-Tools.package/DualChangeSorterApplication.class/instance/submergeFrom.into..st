menu - change set
submergeFrom: src into: dest
	"Copy the contents of the receiver to the other side, then remove the receiver -- all after checking that all is well."
	| source destination index |

	source := src selectedChangeSet.
	destination := dest selectedChangeSet.
	index := src selectedChangeSetIndex.

	(self model submerge: source into: destination) ifFalse: [ ^ self ].
	
	src updateChangesList.
	src setSelectedChangeSetIndex: index.