initialization
initializeWidgets
	"Widgets instanciation"

	self instantiateModels: #(#label #LabelModel #textInput #TextInputFieldModel).

	"Widgets configuration specification"
	self title: 'Choose your project'.
	self label label: 'Packages :'.
	self textInput
		autoAccept: true;
		entryCompletion: nil;
		ghostText: '.*';
		bindKeyCombination: Character cr toAction: [ self toolbar triggerOkAction ].

	"Specification of order of focus"
	self focusOrder add: self textInput.

	"Call the method to define dynamically the layout"
	self dynamicLayout.