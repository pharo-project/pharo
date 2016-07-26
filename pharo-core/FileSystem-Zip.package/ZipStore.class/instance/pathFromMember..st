private
pathFromMember: anArchiveMember
	| path |
	path := Path from: anArchiveMember fileName.
	^ path basename isEmpty
		ifTrue: [ path parent ]
		ifFalse: [ path ]