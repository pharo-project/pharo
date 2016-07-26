accessing
sortedCounts
	"Answer with a collection of counts with elements, sorted by decreasing
	count."
	"Suggested by l. Uzonyi"

	^(Array new: contents size streamContents: [ :stream |
 			contents associationsDo: [ :each |
 				stream nextPut: each value -> each key ] ])
 		sort: [:x :y | x >= y ];
 		yourself