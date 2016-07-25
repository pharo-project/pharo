updating screen
updateOnOSWindow
	"Update this world's display."

	| handsToDraw allDamage worldDamageRects handDamageRects canvas |

	submorphs do: [:m | m fullBounds].  "force re-layout if needed"
	worldState checkIfUpdateNeeded ifFalse: [^ self].  "display is already up-to-date"

	canvas := self osWindowRenderer getCanvas.
	
	self osWindowRenderer deferUpdatesWhile: [  
	
		worldDamageRects := worldState drawWorld: self submorphs: submorphs invalidAreasOn: canvas.  "repair world's damage on canvas"
		"self handsDo:[:h| h noticeDamageRects: worldDamageRects]."
		handsToDraw := worldState selectHandsToDrawForDamage: worldDamageRects.
		handDamageRects := handsToDraw collect: [:h | h savePatchFrom: canvas].
		allDamage := worldDamageRects, handDamageRects.

		handsToDraw reverseDo: [:h | canvas fullDrawMorph: h].  "draw hands onto world canvas"
 
	"*make this true to flash damaged areas for testing*"
		WorldState debugShowDamage ifTrue: [self flashRects: allDamage color: Color black].

		canvas finish.
	"quickly copy altered rects of canvas to Display:"
	
 
		self osWindowRenderer updateAreas: allDamage immediate: false.

 		handsToDraw do: [:h | h restoreSavedPatchOn: canvas].  "restore world canvas under hands"
	].
"	self osWindowRenderer forceDisplayUpdate."
	