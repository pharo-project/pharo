initialize
receiver: anObject selector: aSymbol argument: aParameter
	"Not really necessary now that we're a subclass of MessageSend, but we keep it around because existing code might expect it."
	self 
		receiver: anObject;
		selector: aSymbol;
		arguments: (Array with: aParameter).