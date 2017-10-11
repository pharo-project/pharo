event-handling
saveElement: newText
	"When you press Cmd+s on the description TextPresenter, you can save the new value in the inspected object"

	|newValue|
		
	newValue := description doItReceiver class compiler
		source: newText;
		context: self doItContext;
		receiver: description doItReceiver;
		failBlock: [^ self];
		evaluate.
	
	self selectedElement save: newValue.
	self objectChanged.