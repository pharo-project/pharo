history behavior
previous
	self model hasPrevious 
		ifFalse: [ ^ self ].
	"show the previous item in the history"
	self model previous