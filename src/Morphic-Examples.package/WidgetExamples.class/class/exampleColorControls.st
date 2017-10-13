examples
exampleColorControls
	"Open a window with examples of each of the color controls."
	"self exampleColorControls"
	<script>
	| dialog builder|
	builder := self exampleBuilder.
	dialog := (builder newPluggableDialogWindow: 'Example color controls') useDefaultOKButton.
	dialog contentMorph: (dialog newRow: {
		dialog newLabelGroup: {
			'Alpha Selector'->(dialog newAlphaSelector: (NewValueHolder new value: 0)
							getAlpha: #value setAlpha: #value: help: 'This is an alpha channel selector').
			'Hue Selector'->(dialog newHueSelector: (NewValueHolder new value: 0)
							getHue: #value setHue: #value: help: 'This is a hue selector').
			'SV Color Selector'->((dialog newSVSelector: Color yellow
									help: 'This is a saturation-volume selector') minHeight: 80).
			'HSV Color Selector'->((dialog newHSVSelector: Color green
									help: 'This is a hue-saturation-volume selector') minHeight: 80; minWidth: 120)}.
		dialog newVerticalSeparator.
		dialog newTitle: 'HSVA Color Selector' for:
			((dialog newHSVASelector: Color cyan
					help: 'This is a hue-saturation-volume-alpha selector')
					minHeight: 184; minWidth: 184)});
		model: nil.
	builder openModal: dialog