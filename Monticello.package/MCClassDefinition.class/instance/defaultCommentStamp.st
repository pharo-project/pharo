initializing
defaultCommentStamp
	^ String new

	"The version below avoids stomping on stamps already in the image

	^ (Smalltalk globals at: name ifPresent: [:c | c organization commentStamp])
		ifNil: ['']
	"
