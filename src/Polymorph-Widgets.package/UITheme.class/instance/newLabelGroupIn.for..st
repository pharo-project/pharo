morph creation
newLabelGroupIn: aThemedMorph for: labelsAndControls
	"Answer a morph laid out with a column of labels and a column of associated controls.
	The vResizing for each row will be #spaceFill if the control for that row specifies
	this, otherwise #shrinkWrap."

	| labels labelWidth lc |
	lc := labelsAndControls collect: [:a |
		(a key isMorph
			ifTrue: [a key]
			ifFalse: [a key ifNotNil: [self newLabelIn: aThemedMorph label: a key asString]])
			-> a value].
	labels := Morph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		changeTableLayout.
	lc do: [:a |
		a key ifNotNil: [labels addMorphBack: a key]].
	labelWidth := labels minExtent x.
	^self newColumnIn: aThemedMorph for: (lc collect: [:a | | row |
		a key ifNotNil: [a key hResizing: #rigid; extent: labelWidth@ a key height].
		row := self newRowIn: aThemedMorph for: (a key ifNil: [{a value}] ifNotNil: [{a key. a value}]).
		row vResizing: (a value vResizing = #spaceFill ifTrue: [#spaceFill] ifFalse: [#shrinkWrap]).
		row])