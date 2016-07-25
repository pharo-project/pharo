update cycle
displayWorld: aWorld submorphs: submorphs
	"Update this world's display."

	| deferredUpdateMode handsToDraw allDamage |

	submorphs do: [:m | m fullBounds].  "force re-layout if needed"
	self checkIfUpdateNeeded ifFalse: [^ self].  "display is already up-to-date"

	deferredUpdateMode := self doDeferredUpdatingFor: aWorld.
	deferredUpdateMode ifFalse: [self assuredCanvas].
	canvas roundCornersOf: aWorld during:[ | worldDamageRects handDamageRects |
		worldDamageRects := self drawWorld: aWorld submorphs: submorphs invalidAreasOn: canvas.  "repair world's damage on canvas"
		"self handsDo:[:h| h noticeDamageRects: worldDamageRects]."
		handsToDraw := self selectHandsToDrawForDamage: worldDamageRects.
		handDamageRects := handsToDraw collect: [:h | h savePatchFrom: canvas].
		allDamage := worldDamageRects, handDamageRects.

		handsToDraw reverseDo: [:h | canvas fullDrawMorph: h].  "draw hands onto world canvas"
	].
	"*make this true to flash damaged areas for testing*"
	self class debugShowDamage ifTrue: [aWorld flashRects: allDamage color: Color black].

	canvas finish.
	"quickly copy altered rects of canvas to Display:"
	deferredUpdateMode
		ifTrue: [self forceDamageToScreen: allDamage]
		ifFalse: [canvas showAt: aWorld viewBox origin invalidRects: allDamage].
	handsToDraw do: [:h | h restoreSavedPatchOn: canvas].  "restore world canvas under hands"
	Display deferUpdates: false; forceDisplayUpdate.
