printing
nodePrintOn: aStrm indent: nn
	| var aaStrm myLine |
	"Show just the sub nodes and the code."

	(aaStrm := aStrm) ifNil: [aaStrm := (String new: 500) writeStream].
	nn timesRepeat: [aaStrm tab].
	aaStrm nextPutAll: self class name; space.
	myLine := self printString copyWithout: Character cr.
	myLine := myLine copyFrom: 1 to: (myLine size min: 70).
	aaStrm nextPutAll: myLine; cr.
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var respondsTo: #asReturnNode) ifTrue: [var nodePrintOn: aaStrm indent: nn+1]].
	1 to: self class instSize do: [:ii | 
		var := self instVarAt: ii.
		(var isKindOf: SequenceableCollection) ifTrue: [
				var do: [:aNode | 
					(aNode respondsTo: #asReturnNode) ifTrue: [
						aNode nodePrintOn: aaStrm indent: nn+1]]]].
	^ aaStrm
