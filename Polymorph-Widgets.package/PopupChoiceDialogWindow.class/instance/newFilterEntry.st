as yet unclassified
newFilterEntry
	"Answer a new filter entry field."

	|entry|
	entry := self
		newAutoAcceptTextEntryFor: self
		getText: #filter
		setText: #filter:
		getEnabled: nil
		help: 'Filters the options according to a matching substring' translated.
	entry acceptOnCR: false.
	entry textMorph crAction: (MessageSend receiver: self selector: #ok).
	^entry 
