utilities
indexForInserting: aMorph at: aPoint in: owner 
	"Return the insertion index based on the layout strategy defined for some morph. Used for drop insertion."

	| horizontal morphList index |
	owner hasSubmorphs ifFalse: [^1].
	aMorph disableTableLayout ifTrue: [^1].
	horizontal := (owner listDirection == #topToBottom 
				or: [owner listDirection == #bottomToTop]) not .
	morphList := owner submorphs.
	owner reverseTableCells ifTrue: [morphList := morphList reversed].
	index := self 
				indexForInserting: aPoint
				inList: morphList
				horizontal: horizontal
				target: owner.
	owner reverseTableCells ifTrue: [index := morphList size - index + 2].
	^index ifNil: [1]