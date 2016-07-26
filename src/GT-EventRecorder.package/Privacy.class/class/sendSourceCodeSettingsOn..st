settings
sendSourceCodeSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #sendSourceCode)
		label: 'Include source code';
		noOrdering;
		description: 'Help the Pharo community improve its products and services by automatically sending source code as part of the diagnostic and usage data, e.g., changed methods, browsed source code, written code snippets.';
		default: false;
		parent: #sendDiagnosticsAndUsageData