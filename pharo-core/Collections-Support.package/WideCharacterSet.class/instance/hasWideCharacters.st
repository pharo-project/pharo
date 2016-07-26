testing
hasWideCharacters
	"Answer true if i contain any wide character"
	
	self do: [:e | e asciiValue >= 256 ifTrue: [^true]].
	^false