label
setLabelFont: aFont

	label ifNil: [^ self].
	label font: aFont.
