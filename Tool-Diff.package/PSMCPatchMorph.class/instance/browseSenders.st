actions
browseSenders
	"Browse the method senders."

	self systemNavigation
		browseAllSendersOf: (self selectedMessageName ifNil: [^self])