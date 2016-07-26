history behavior
next
	self model hasNext 
		ifFalse: [ ^ self ].
	"show the next item in history"
	self model next