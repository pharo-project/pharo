*Polymorph-Widgets
layoutInBounds: cellBounds
	"Layout specific. Apply the given bounds to the receiver after being layed out in its owner."
	| box aSymbol |
	
	cellBounds = self bounds ifTrue:[^self]. "already up to date. Fixed here to use bounds rather than fullBounds for the check."
	cellBounds extent = self bounds extent "nice fit. Fixed here to use bounds rather than fullBounds for the check."
		ifTrue:[^self position: cellBounds origin].
	box := bounds.
	self hResizing == #shrinkWrap
		ifTrue:[box := box origin extent: self minExtent x @ box height].
	self vResizing == #shrinkWrap
		ifTrue:[box := box origin extent: box width @ self minExtent y].
	"match #spaceFill constraints"
	self hResizing == #spaceFill 
		ifTrue:[box := box origin extent: cellBounds width @ box height].
	self vResizing == #spaceFill
		ifTrue:[box := box origin extent: box width @ cellBounds height].
	"align accordingly"
	aSymbol := (owner ifNil:[self]) cellPositioning.
	box := box align: (box perform: aSymbol) with: (cellBounds perform: aSymbol).
	"and install new bounds"
	self bounds: box.