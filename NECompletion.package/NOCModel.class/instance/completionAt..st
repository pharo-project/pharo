accessing
completionAt: aNumber
	"when an entry is selected, update its date (for inlined messages which do not appear in the method's body"
	| entry |
	
	entry := (self at: aNumber) completion.
	self addEntry: entry.
	
	^ NECPreferences spaceAfterCompletion 
		ifTrue: [ entry separateKeywords, ' ' ]
		ifFalse: [ entry separateKeywords ].