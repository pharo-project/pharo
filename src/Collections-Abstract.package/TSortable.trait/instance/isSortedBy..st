sorting
isSortedBy: aBlock
	"Return true if the receiver is sorted by the given criterion."

	| lastElm elm |
	self isEmpty ifTrue: [^ true].
	lastElm := self first.
	2 to: self size do: 
		[:index | 
		elm := self at: index.
		(aBlock value: lastElm value: elm) ifFalse: [^ false].
		lastElm := elm].
	^ true