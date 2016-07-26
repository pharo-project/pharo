actions
updateSettings
	
	autoSubmit state ifTrue: [
		SlReporter
			autoSubmit: true;
			showSummary: false;
			showNotification: false ]