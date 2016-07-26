morph creation
newLabelGroupIn: aThemedMorph for: labelsAndControls font: aFont labelColor: aColor
	"Answer a morph laid out with a column of labels and a column of associated controls.
	If spaceFill is tru then each row will share available space to pad."

	| labels labelWidth lc|
	lc := labelsAndControls collect: [:a |
		(a key isMorph
			ifTrue: [a key]
			ifFalse: [(self newLabelIn: aThemedMorph label: (a key ifNil: ['']) asString)
						font: aFont;
						color: aColor])
			-> a value].
	labels := Morph new
		hResizing: #shrinkWrap;
		vResizing: #spaceFill;
		changeTableLayout.
	lc do: [:a |
		labels addMorphBack: a key].
	labelWidth := labels minExtent x.
	^self newColumnIn: aThemedMorph for: (lc collect: [:a | | row |
		a key hResizing: #rigid; extent: labelWidth@ a key height.
		row := self newRowIn: aThemedMorph for: {a key. a value}.
		row vResizing: (a value vResizing = #spaceFill ifTrue: [#spaceFill] ifFalse: [#shrinkWrap]).
		row])