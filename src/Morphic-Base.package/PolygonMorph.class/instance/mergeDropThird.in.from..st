geometry
mergeDropThird: mv in: hv from: shared 
	"We are merging two polygons.  In this case, they have at least three identical shared vertices.  Make sure they are sequential in each, and drop the middle one from vertex lists mv, hv, and shared.  First vertices on lists are identical already."

	"know (mv first = hv first)"

	| mdrop vv |
	(shared includes: (mv at: mv size - 2)) 
		ifTrue: [(shared includes: mv last) ifTrue: [mdrop := mv last]]
		ifFalse: 
			[(shared includes: mv last) 
				ifTrue: [(shared includes: mv second) ifTrue: [mdrop := mv first]]].
	(shared includes: (mv third)) 
		ifTrue: [(shared includes: mv second) ifTrue: [mdrop := mv second]].
	mdrop ifNil: [^nil].
	mv remove: mdrop.
	hv remove: mdrop.
	shared remove: mdrop.
	[shared includes: mv first] whileFalse: 
			["rotate them"

			vv := mv removeFirst.
			mv addLast: vv].
	[mv first = hv first] whileFalse: 
			["rotate him until same shared vertex is first"

			vv := hv removeFirst.
			hv addLast: vv]