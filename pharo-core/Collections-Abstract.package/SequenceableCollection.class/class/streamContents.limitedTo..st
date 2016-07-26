stream creation
streamContents: blockWithArg limitedTo: sizeLimit
	| stream |
	stream := LimitedWriteStream on: (self streamSpecies new: (100 min: sizeLimit)).
	stream setLimit: sizeLimit limitBlock: [^ stream contents].
	blockWithArg value: stream.
	^ stream contents
"
String streamContents: [:s | 1000 timesRepeat: [s nextPutAll: 'Junk']] limitedTo: 25
 'JunkJunkJunkJunkJunkJunkJ'
"