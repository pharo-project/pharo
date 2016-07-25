accessing
packagesSelection: aCollection
	"I change the classes selection, then I rebuild the result dictionary"
	packagesSelection :=  aCollection.
	self constructDictionary.