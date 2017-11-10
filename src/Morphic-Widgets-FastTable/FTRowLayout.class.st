"
I'm a layout specifphic for rows in a fast table.
I simplify my parent (in an attept to achieve speed), but most important, Itake into account cells width.
"
Class {
	#name : #FTRowLayout,
	#superclass : #RowLayout,
	#category : #Morphic-Widgets-FastTable
}

{ #category : #layouting }
FTRowLayout >> layout: aMorph in: bounds [
	"Compute the layout for the given morph based on the new bounds.
	Supports submorph hResizing, vResizing, cellInset, cellPositioning
	(top, *center, bottom*) and listCentering."
	| props spare extra height inset fillCount x width |

	aMorph submorphs ifEmpty: [ ^ self ].
	props := aMorph assureTableProperties.
	width := (self minExtentOf: aMorph in: bounds) x.
	spare := bounds width - width max: 0.
	fillCount := 0.
	spare > 0 
		ifTrue: [
			fillCount := aMorph submorphs size.
			extra := (fillCount = 0) ifTrue: [0] ifFalse: [ spare // fillCount ].
			spare := spare - ((fillCount - 1) * extra) ]
		ifFalse: [ extra := 0 ].
		
	x := fillCount > 0
		ifTrue: [ bounds left ]
		ifFalse: [
			props listCentering == #center
				ifTrue: [ bounds center x - (width // 2) ]
				ifFalse: [
					props listCentering == #bottomRight
						ifTrue: [ bounds right - width ]
						ifFalse: [ bounds left ] ] ].
	
	height := bounds height.
	inset := props cellInset isPoint ifTrue: [ props cellInset x ] ifFalse: [ props cellInset ]. 
	aMorph submorphs withIndexDo: [ :eachMorph :index | | pos box cell |
		width := ((eachMorph width = 0) and: [ fillCount > 0 ]) 
			ifTrue: [ (bounds width + bounds left - x) / (fillCount - index + 1) ]
			ifFalse: [ eachMorph width ].
		cell := x@bounds top extent: width@height.
		box := cell origin extent: cell width @ cell height.		
		pos := props cellPositioning.
		box := box align: (box perform: pos) with: (cell perform: pos).
		eachMorph bounds: box.
		x := x + width + inset ]
]
