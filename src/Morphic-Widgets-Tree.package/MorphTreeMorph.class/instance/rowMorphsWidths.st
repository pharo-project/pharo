column handling
rowMorphsWidths
	"Return all row morphs witdhs based on the header bounds. 
	used when a resizer is moved horizontally or if some 
	change implies that the list is rebuilt"
	| result |
	result :=  self headerBounds collect: [:b | b width].
	result ifNotEmpty: [result at: 1 put: ((result at: 1) - 3 )].
	^ result
