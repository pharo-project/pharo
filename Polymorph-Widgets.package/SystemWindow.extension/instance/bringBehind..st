*Polymorph-Widgets
bringBehind: aMorph
	"Make the receiver be directly behind the given morph.
	Take into account any modal owner and propagate."

	|outerMorph|
	outerMorph := self topRendererOrSelf.
	outerMorph owner ifNil: [^ self "avoid spurious activate when drop in trash"].
	outerMorph owner addMorph: outerMorph after: aMorph topRendererOrSelf.
	self modalOwner ifNotNil: [:mo | mo bringBehind: self]