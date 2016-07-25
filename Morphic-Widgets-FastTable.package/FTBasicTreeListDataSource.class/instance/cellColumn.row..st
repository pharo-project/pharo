accessing
cellColumn: column row: rowIndex
	| item |
	
	"this is a bad way for tree; maybe list too. the morph should ask for the full collection from index start to n elements. 
	eps when elementAt: must traverse the tree to find each element: not only a basic at:
	"
	item := self elementAt: rowIndex.

	^item cellMorphFor: self.