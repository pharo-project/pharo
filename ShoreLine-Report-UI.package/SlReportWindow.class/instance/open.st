actions
open

	SlReporter showSummary
		ifTrue: [ self openWithSpec ]
		ifFalse: [ self submit ]