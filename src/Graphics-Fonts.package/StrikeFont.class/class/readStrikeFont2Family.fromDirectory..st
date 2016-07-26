examples
readStrikeFont2Family: familyName fromDirectory: aDirectory 
	"StrikeFont readStrikeFont2Family: 'Lucida' fromDirectory: FileDirectory default"
	"This utility reads all available .sf2 StrikeFont files for a given family from  
	the current directory. It returns an Array, sorted by size, suitable for handing 
	to TextStyle newFontArray: ."
	"For this utility to work as is, the .sf2 files must be named 'familyNN.sf2'."
	| files strikeFonts fontArray |
	files := aDirectory filesMatching: familyName , '##.sf2'.
	strikeFonts := files collect: [ :file | StrikeFont new readFromStrike2: file basename ].
	strikeFonts do: [ :font | font reset ].
	strikeFonts := strikeFonts asSortedCollection: [ :a :b | a height < b height ].
	fontArray := strikeFonts asArray.
	^ fontArray

	"TextConstants at: #Lucida put: (TextStyle fontArray: (StrikeFont 
	readStrikeFont2Family: 'Lucida'))."