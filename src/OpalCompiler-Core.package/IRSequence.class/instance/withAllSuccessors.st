successor sequences
withAllSuccessors
	"Return me and all my successors sorted by sequence orderNumber"

	| list |
	list := OrderedCollection new: 20.
	self withAllSuccessorsDo: [:seq | list add: seq].
	^ list asSortedCollection: [:x :y | x orderNumber <= y orderNumber]