as yet unclassified
newButtons
	"Answer new buttons as appropriate."

	self filterMorph: self newFilterEntry.
	^{self filterMorph. self newCancelButton}