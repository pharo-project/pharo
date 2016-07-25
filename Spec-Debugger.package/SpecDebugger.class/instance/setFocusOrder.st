initialization
setFocusOrder

	self focusOrder 
		removeAll;
		add: stack;
		add: toolbar;
		add: code;
		add: receiverInspector;
		add: contextInspector