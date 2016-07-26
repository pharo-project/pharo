accessing
size
	"count each non nil elements in self.
	Note: count: will use do: which will already have filtered out nil elements"
	
	^self count: [:each | true]