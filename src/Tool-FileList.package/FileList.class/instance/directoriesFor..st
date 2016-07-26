private
directoriesFor: directory
	"item may be file directory or server directory"
	| entries |
	entries := directory directories.
	dirSelectionBlock ifNotNil:[ entries := entries select: dirSelectionBlock ].
	^entries