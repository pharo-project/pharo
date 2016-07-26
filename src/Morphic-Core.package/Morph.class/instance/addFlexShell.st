rotate scale and flex
addFlexShell
	"Wrap a rotating and scaling shell around this morph."

	| oldHalo flexMorph myWorld anIndex |

	myWorld := self world.
	oldHalo := self halo.
	anIndex := self owner submorphIndexOf: self.
	self owner addMorph: (flexMorph := self newTransformationMorph asFlexOf: self)
		asElementNumber: anIndex.
	self transferStateToRenderer: flexMorph.
	oldHalo ifNotNil: [oldHalo setTarget: flexMorph].
	myWorld ifNotNil: [myWorld startSteppingSubmorphsOf: flexMorph].

	^ flexMorph