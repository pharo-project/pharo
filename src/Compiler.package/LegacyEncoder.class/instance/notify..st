error handling
notify: string
	"Put a separate notifier on top of the requestor's window"
	| req |
	requestor == nil
		ifTrue:
			[super notify: string]
		ifFalse: 
			[req := requestor.
			self release.
			req notify: string].
	^false