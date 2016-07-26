as yet unclassified
fitBaseMorphs
	"Replace the receiver's submorphs with the base morphs that
	can fit in the receiver's bounds. Critically, morphs must be replaced in
	one go to avoid #extent: recursion."

	|proposed|
	self hideMore.
	proposed := self newPotentialMorph.
	[proposed width <= self width or: [proposed submorphCount = 0]] whileFalse: [
		proposed
			removeMorph: proposed lastSubmorph;
			extent: proposed minExtent].
	proposed submorphCount < self baseMorphs size ifTrue: [
		self moreButton hResizing: #shrinkWrap.
		proposed
			addMorphBack: self moreButton;
			extent: proposed minExtent].
	[proposed width > self width and: [proposed submorphCount > 1]] whileTrue: [
		proposed
			removeMorph: (proposed submorphs at: proposed submorphs size - 1);
			extent: proposed minExtent].
	(proposed submorphCount = 1 and: [self moreButton owner notNil]) ifTrue: [
		self moreButton hResizing: #spaceFill].
	self
		removeAllMorphs;
		addAllMorphs: proposed submorphs
	