event handling
mouseEnterDragging: evt
	"The mouse has entered with a button down.
	Workaround for apparent flaw in MouseOverHandler constantly
	sending this message when dragging.
	Do nothing if disabled."
	
	|row oldPDR|
	self enabled ifFalse: [^self].
	row := self rowAtLocation: evt position.
	(self dragEnabled or: [evt hand hasSubmorphs]) ifFalse: [
		row = 0 ifTrue: [ ^self ].
		self listMorph mouseDownRow: row].
	(evt hand hasSubmorphs and:[self dropEnabled]) ifFalse: ["no d&d"
		^super mouseEnterDragging: evt].
	potentialDropRow = row ifTrue: [^self].
	oldPDR := potentialDropRow.
	potentialDropRow := row.
	evt hand newMouseFocus: self.
	"above is ugly but necessary for now"
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt )
		ifTrue: [self changed]
		ifFalse: [(oldPDR ifNil: [0]) > 0
				ifTrue: [self resetPotentialDropRow]
				ifFalse: [potentialDropRow := 0]]