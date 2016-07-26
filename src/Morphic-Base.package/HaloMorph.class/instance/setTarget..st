accessing
setTarget: aMorph
	"Private! Set the target without adding handles."

	target := aMorph topRendererOrSelf.
	innerTarget := target renderedMorph.
	innerTarget wantsDirectionHandles
		ifTrue: [self showDirectionHandles: true addHandles: false].
	target hasHalo: true.

