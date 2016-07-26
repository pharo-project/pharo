sorting
isSorted
	"Return true if the receiver is sorted by the given criterion.
	Optimization for isSortedBy: [:a :b | a <= b]."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		lastElm <= elm ifFalse: [^ false].
		lastElm := elm].
	^ true