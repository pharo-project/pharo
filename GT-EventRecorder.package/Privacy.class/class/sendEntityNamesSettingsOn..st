settings
sendEntityNamesSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #sendEntityNames)
		label: 'Include package/class/method names';
		noOrdering;
		description: 'Help the Pharo community improve its products and services by automatically sending package, class, or method names as part of the diagnostic and usage data, e.g., browsed classes, changed methods.';
		default: false;
		parent: #sendDiagnosticsAndUsageData