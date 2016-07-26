printing
printString
	"For Integer, prefer the stream version to the string version for efficiency"
	
	^String streamContents: [:str | self printOn: str base: 10]