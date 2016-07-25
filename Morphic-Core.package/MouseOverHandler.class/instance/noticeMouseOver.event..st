event handling
noticeMouseOver: aMorph event: anEvent
	"Remember that the mouse is currently over some morph"

	leftMorphs remove: aMorph ifAbsent: [ enteredMorphs nextPut: aMorph ].
	overMorphs nextPut: aMorph.
