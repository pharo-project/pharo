system primitives
nextInstance
	"SmallIntegers can't be enumerated this way.  There are a finite number of them from from (SmallInteger minVal) to (SmallInteger maxVal), but you'll have to enumerate them yourself with:
	(SmallInteger minVal) to: (SmallInteger maxVal) do: [:integer | <your code here>].
	"

	self shouldNotImplement 