initialization
extendByHand: aHand
	"Assumes selection has just been created and added to some pasteUp or world"
	| startPoint handle |

	startPoint := self position.

	handle := NewHandleMorph new followHand: aHand
		forEachPointDo: [:newPoint |
					| localPt |
					localPt := (self transformFrom: self world) globalPointToLocal: newPoint.
					self bounds: (startPoint rectangle: localPt)
				]
		lastPointDo: [:newPoint |
					selectedItems isEmpty
						ifTrue: [self delete]
						ifFalse: [
							selectedItems size = 1
								ifTrue:[self delete.  selectedItems anyOne addHalo]
								ifFalse:[self doneExtending]
						]
				].

	aHand attachMorph: handle.
	handle startStepping.