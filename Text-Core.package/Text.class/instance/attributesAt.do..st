emphasis
attributesAt: characterIndex do: aBlock
	"Answer the code for characters in the run beginning at characterIndex."
	"NB: no senders any more (supplanted by #attributesAt:forStyle: but retained for the moment in order not to break user code that may exist somewhere that still calls this"
	self size = 0 ifTrue:[^self].
	(runs at: characterIndex) do: aBlock