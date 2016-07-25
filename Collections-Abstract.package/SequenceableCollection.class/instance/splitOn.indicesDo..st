splitjoin
splitOn: splitter indicesDo: aBlock
	"splitter - can be a subsequence, a Block or a Regex (String receiver only).
	Any other object used as a splitter is treated as an Array containing that object."
	^ splitter split: self indicesDo: aBlock