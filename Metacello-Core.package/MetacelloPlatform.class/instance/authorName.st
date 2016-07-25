utilities
authorName

	Smalltalk at: #Author ifPresent: [:cl | ^cl perform: #initials ].
	^'no developer initials'