change reporting
privateInvalidateMorph: aMorph
	"Private. Invalidate the given morph after adding or removing.
	This method is private because a) we're invalidating the morph 'remotely'
	and b) it forces a fullBounds computation which should not be necessary
	for a general morph c) the morph may or may not actually invalidate
	anything (if it's not in the world nothing will happen) and d) the entire
	mechanism should be rewritten."
	aMorph fullBounds.
	aMorph changed