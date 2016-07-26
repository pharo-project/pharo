settings
sendDiagnosticsAndUsageUsageDataSettingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #sendDiagnosticsAndUsageData)
		label: 'Send diagnostics and usage data';
		noOrdering;
		description: 'Help the Pharo community improve its products and services by automatically sending diagnostic and usage data.';
		default: false;
		parent: #privacy