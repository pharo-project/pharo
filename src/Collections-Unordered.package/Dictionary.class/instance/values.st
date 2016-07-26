accessing
values
	"Answer a Collection containing the receiver's values."
	| out |
	out := (Array new: self size) writeStream.
	self valuesDo: [:value | out nextPut: value].
	^ out contents