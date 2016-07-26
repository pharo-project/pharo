private
image: aForm at: aPoint sourceRect: sourceRect rule: rule
	"Note: The public use of this protocol is deprecated. It will become private. Nobody in the outside world must assume that a thing like a combination rule has any specific effect."
	^self subclassResponsibility