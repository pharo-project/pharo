examples
exampleDisableInset
	"self exampleDisable"

	<exampleWidget>
	^ (self newLabel: 'A Label')
		disable;
		disabledStyle: #inset;
		changeFont;
		openInWindow