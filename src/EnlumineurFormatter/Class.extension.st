Extension { #name : 'Class' }

{ #category : '*EnlumineurFormatter' }
Class >> reformatAll [
	"Reformat all methods in this class.
	Leaves old code accessible to version browsing"

	super reformatAll. "me..."
	self classSide reformatAll "...and my metaclass"
]
