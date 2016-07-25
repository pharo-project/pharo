as yet unclassified
run

	"partition nodes by connectivity"
	self nodes do: [:n|
		n nextNodes do: [:next | 
			n union: next ]].
	components := Dictionary new.
	self nodes inject: components into: [:d :n |
		(d at: n find ifAbsentPut: [OrderedCollection new]) add: n. d ]