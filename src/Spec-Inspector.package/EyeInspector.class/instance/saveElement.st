event-handling
saveElement
	"When you press Cmd+s on the description TextPresenter, you can save the new value in the inspected object"

	|newValue|
		
	newValue := description doItReceiver class compiler		
		source: description text readStream;
		context: nil "should be object in case of methodContext";
		receiver:  description doItReceiver;
		failBlock: [^ self];
		evaluate.
		
	self selectedElement save: newValue.
	self objectChanged