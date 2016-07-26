updating widgets
updateCodeFromContext: aContext 

	aContext 
		ifNil: [
			self code 
				aboutToStyle: false;
				text: '';
				behavior: nil;
				setSelection: (1 to: 0) ]
		ifNotNil: [ |selectionInterval|
			selectionInterval := self session selectedCodeRangeForContext: aContext.
			self code
				aboutToStyle: true;
				text: aContext sourceCode;
				doItReceiver: aContext home receiver;
				doItContext: aContext;
				behavior: aContext home receiver class;
				setSelection: selectionInterval ]