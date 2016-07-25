border-styles
progressBarBorderStyleFor: aProgressBar
	"Return the progress bar borderStyle for the given progress bar."

	|c|
	c := self progressBarColorFor: aProgressBar.
	^BorderStyle simple
		width: 1;
		baseColor: c darker