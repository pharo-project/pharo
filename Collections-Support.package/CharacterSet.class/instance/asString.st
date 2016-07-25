conversion
asString
	"Convert the receiver into a String"

	^String 
		new: self size 
		streamContents: 
			[ :s | self do: [ :ch | s nextPut: ch]].