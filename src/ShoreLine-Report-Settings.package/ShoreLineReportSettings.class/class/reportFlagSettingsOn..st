settings
reportFlagSettingsOn: aBuilder

	(aBuilder setting: #enabled)
		target: SlReporter;
		label: 'Enable ShoreLine Reporter';
		description: 'Enable the submission of automaticalle generated reports';
		parent: #shoreLine.

	(aBuilder setting: #autoSubmit)
		target: SlReporter;
		label: 'Automatic submission';
		description: 'If this is enabled, a report is automatically submitted every time an exception occurs';
		parent: #shoreLine.
		
	(aBuilder setting: #showSummary)
		target: SlReporter;
		label: 'Show report summary';
		description: 'Display a summary of the data before submission';
		parent: #shoreLine.
		
	(aBuilder setting: #showNotification)
		target: SlReporter;
		label: 'Show notifications';
		description: 'Show a notification popup after a report ha been submitted';
		parent: #shoreLine.
