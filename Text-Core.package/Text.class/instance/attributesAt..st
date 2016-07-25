emphasis
attributesAt: characterIndex 
	"Answer the code for characters in the run beginning at characterIndex."
	"NB: no senders any more (supplanted by #attributesAt:forStyle: but retained for the moment in order not to break user code that may exist somewhere that still calls this"
	| attributes |
"	self size = 0
		ifTrue: [^ Array with: (TextFontChange new fontNumber: 1)]."
	self size = 0 ifTrue: [ ^#()].
	attributes := runs at: characterIndex.
	^ attributes