Extension { #name : #PluggableTextMorph }

{ #category : #'*Athens-Morphic' }
PluggableTextMorph >> drawOnAthensCanvas: anAthensCanvas [
	"Indicate unaccepted edits, conflicts etc."

	super drawOnAthensCanvas: anAthensCanvas. 
	self wantsFrameAdornments ifTrue: [
		self theme drawTextAdornmentsFor: self on: anAthensCanvas asCanvasWrapper].
	self drawHighlightsOn: anAthensCanvas asCanvasWrapper
]
