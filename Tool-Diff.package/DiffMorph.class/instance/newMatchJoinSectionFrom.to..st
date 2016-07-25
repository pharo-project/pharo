instance-creation
newMatchJoinSectionFrom: srcRange to: dstRange
	"Answer a new match join section."

	| spl dpl sy1 sy2 dy1 dy2 c |
	spl :=self srcMorph textMorph paragraph lines.
	dpl := self dstMorph textMorph paragraph lines.
	sy1 := (spl at: srcRange first) top truncated.
	sy2 := (spl at: srcRange last) bottom truncated.
	dy1 := (dpl at: dstRange first) top truncated.
	dy2 := (dpl at: dstRange last) bottom truncated.
	c := self colorForType: #match.
	^ self newJoinSection
		type: #match;
		borderWidth: 0;
		srcColor: c;
		dstColor: c;
		srcLineRange: srcRange;
		dstLineRange: dstRange;
		srcRange: (sy1 to: sy2);
		dstRange: (dy1 to: dy2);
		createHighlightsFrom: self srcMorph textMorph paragraph to: self dstMorph textMorph paragraph