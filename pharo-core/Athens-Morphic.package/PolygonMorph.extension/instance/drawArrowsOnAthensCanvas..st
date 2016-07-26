*Athens-Morphic
drawArrowsOnAthensCanvas: aAthensCanvas 
	"Answer (possibly modified) endpoints for border drawing"
	"ArrowForms are computed only upon demand"
	| array |

	self hasArrows
		ifFalse: [^ #() ].
	"Nothing to do"

	array := Array with: vertices first with: vertices last.

	"Prevent crashes for #raised or #inset borders"
	borderColor isColor
		ifFalse: [ ^array ].

	(arrows == #forward or: [arrows == #both])
		ifTrue: [ array at: 2 put: (self
				drawArrowOnAthensCanvas: aAthensCanvas
				at: vertices last
				from: self nextToLastPoint) ].

	(arrows == #back or: [arrows == #both])
		ifTrue: [ array at: 1 put: (self
				drawArrowOnAthensCanvas: aAthensCanvas
				at: vertices first
				from: self nextToFirstPoint) ].

	^array