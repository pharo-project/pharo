testing
ifSpecialKey: aCharacter do: aBlock
	"Lookup if aCharacter needs to be handled by me, if so use one of the instance creation function for creating the corresponding KeyCombination."
	 self specialKeys at: aCharacter ifPresent: [ :symbol |
		aBlock value: (self perform: symbol) ]