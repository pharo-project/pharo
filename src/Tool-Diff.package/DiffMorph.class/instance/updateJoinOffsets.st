updating
updateJoinOffsets
	"Update the src and dst offsets in the join morph
	to match the src and dst tex scroll offsets."

	self joinMorph
		srcOffset: 0 @ self srcMorph scrollPane scroller offset y negated;
		dstOffset: 0 @ self dstMorph scrollPane scroller offset y negated;
		changed