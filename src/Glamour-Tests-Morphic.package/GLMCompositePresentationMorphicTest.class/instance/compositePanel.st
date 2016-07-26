private
compositePanel
	|composite|
	composite := self compositeMorph.
	self assert: (composite isKindOf: PanelMorph).
	self assert: (composite submorphs allSatisfy: [:morph | morph isKindOf: ExpanderMorph]).
	^ composite
