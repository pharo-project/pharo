settings
reportFlagSettingsOn: aBuilder

	(aBuilder setting: #enabled)
		target: SlReporter;
		label: 'Enable ShoreLine Reporter' translated;
		description: 'Enable the submission of automaticalle generated reports' translated;
		parent: #shoreLine.

	(aBuilder setting: #autoSubmit)
		target: SlReporter;
		label: 'Automatic submission' translated;
		description: 'If this is enabled, a report is automatically submitted every time an exception occurs' translated;
		parent: #shoreLine.
		
	(aBuilder setting: #showSummary)
		target: SlReporter;
		label: 'Show report summary' translated;
		description: 'Display a summary of the data before submission' translated;
		parent: #shoreLine.
		
	(aBuilder setting: #showNotification)
		target: SlReporter;
		label: 'Show notifications' translated;
		description: 'Show a notification popup after a report ha been submitted' translated;
		parent: #shoreLine.
