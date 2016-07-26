event handling
mouseEnterDragging: evt
	| aMorph |
	(evt hand hasSubmorphs and:[self dropEnabled]) ifFalse: ["no d&d"
		^super mouseEnterDragging: evt].
	(self wantsDroppedMorph: evt hand firstSubmorph event: evt )
		ifTrue:[
			aMorph := self itemFromPoint: evt position.
			aMorph ifNotNil:[self potentialDropMorph: aMorph].
			evt hand newMouseFocus: self.
			"above is ugly but necessary for now"
		].