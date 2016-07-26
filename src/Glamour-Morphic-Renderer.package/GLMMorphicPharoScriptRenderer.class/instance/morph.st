rendering
morph
	|morph|
	morph := RubScrolledTextMorph new
		model: textModel;
		beForSmalltalkScripting;
		getSelectionSelector: #primarySelectionInterval;
		textFont: StandardFonts codeFont;
		color: Smalltalk ui theme backgroundColor;
		notificationStrategy: GLMPopperNotificationStrategy new;
		yourself.
	"Without forcing styling explicitly the first time the presentation is opened with some text on it the styling is wrong."
	(morph textArea decoratorNamed: #shoutStyler) ifNotNil: [ :styler | styler refreshStyling ].
	^ morph