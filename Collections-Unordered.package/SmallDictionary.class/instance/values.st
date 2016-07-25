accessing - values
values
	"Answer a Collection containing the receiver's values."
	"^ values copyFrom: 1 to: size."
	
| out |
	out := (Array new: self size) writeStream.
	self valuesDo: [:value | out nextPut: value].
	^ out contents