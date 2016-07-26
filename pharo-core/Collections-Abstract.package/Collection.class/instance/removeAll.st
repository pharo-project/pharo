removing
removeAll
	"Remove each element from the receiver and leave it empty.
	ArrayedCollections cannot respond to this message.
	There are two good reasons why a subclass should override this message:
	1) the subclass does not support being modified while being iterated
	2) the subclass provides a much faster way than iterating through each element"

	self do: [:each | self remove: each].