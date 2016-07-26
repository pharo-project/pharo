copying
copyFrom: anotherObject
	"Copy to myself all instance variables I have in common with anotherObject.  This is dangerous because it ignores an object's control over its own inst vars.  "

	| mine his |
	<primitive: 168>
	mine := self class allInstVarNames.
	his := anotherObject class allInstVarNames.
	1 to: (mine size min: his size) do: [:ind |
		(mine at: ind) = (his at: ind) ifTrue: [
			self instVarAt: ind put: (anotherObject instVarAt: ind)]].
	self class isVariable & anotherObject class isVariable ifTrue: [
		1 to: (self basicSize min: anotherObject basicSize) do: [:ind |
			self basicAt: ind put: (anotherObject basicAt: ind)]].