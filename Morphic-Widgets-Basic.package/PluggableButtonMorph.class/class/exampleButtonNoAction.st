examples
exampleButtonNoAction
	<exampleWidget>

	^ PluggableButtonMorph
		newButtonFor: nil
		action: nil
		label: 'A Button'
		help: 'This is a button'