geometry
merge: aPolygon 
	"Expand myself to enclose the other polygon.  (Later merge overlapping or disjoint in a smart way.)  For now, the two polygons must share at least two vertices.  Shared vertices must come one after the other in each polygon.  Polygons must not overlap."

	| shared mv vv hv xx |
	shared := vertices select: [:mine | aPolygon vertices includes: mine].
	shared size < 2 ifTrue: [^nil].	"not sharing a segment"
	mv := vertices asOrderedCollection.
	[shared includes: mv first] whileFalse: 
			["rotate them"

			vv := mv removeFirst.
			mv addLast: vv].
	hv := aPolygon vertices asOrderedCollection.
	[mv first = hv first] whileFalse: 
			["rotate him until same shared vertex is first"

			vv := hv removeFirst.
			hv addLast: vv].
	[shared size > 2] whileTrue: 
			[shared := shared asOrderedCollection.
			(self 
				mergeDropThird: mv
				in: hv
				from: shared) ifNil: [^nil]].
	"works by side effect on the lists"
	(mv second) = hv last 
		ifTrue: 
			[mv
				removeFirst;
				removeFirst.
			^self setVertices: (hv , mv) asArray].
	(hv second) = mv last 
		ifTrue: 
			[hv
				removeFirst;
				removeFirst.
			^self setVertices: (mv , hv) asArray].
	(mv second) = (hv second) 
		ifTrue: 
			[hv removeFirst.
			mv remove: (mv second).
			xx := mv removeFirst.
			^self setVertices: (hv , (Array with: xx) , mv reversed) asArray].
	mv last = hv last 
		ifTrue: 
			[mv removeLast.
			hv removeFirst.
			^self setVertices: (mv , hv reversed) asArray].
	^nil