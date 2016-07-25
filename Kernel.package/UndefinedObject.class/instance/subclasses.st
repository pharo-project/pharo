class hierarchy
subclasses
	"Return all the subclasses of nil"
	| classList |
	classList := Array new writeStream.
	self subclassesDo:[:class| classList nextPut: class].
	^classList contents