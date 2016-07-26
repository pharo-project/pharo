instance creation
newFrom: aCollection
	"Answer an instance with same elements as aCollection."
	^self new
		addAll: aCollection;
		yourself