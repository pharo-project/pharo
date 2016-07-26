*FuelTests
with: aCollectionOrStream do: aBlock
	"Evaluates a block with a new stream based on the collection or stream. Answers the result of the block evaluation. Follows the style of FileStream>>fileNamed:do:."
	| aStream |
	aStream := self on: aCollectionOrStream.
	[ ^ aBlock value: aStream ] ensure: [ aStream close ]