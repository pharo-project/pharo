accessing
updateHighlights
	"Update the highlight border colors."

	|bc|
	(self src isNil or: [self dst isNil]) ifTrue: [^self].
	bc := self borderColorToUse.
	self src highlight notNil ifTrue: [
		self src highlight borderColor: bc].
	self dst highlight notNil ifTrue: [
		self dst highlight borderColor: bc]