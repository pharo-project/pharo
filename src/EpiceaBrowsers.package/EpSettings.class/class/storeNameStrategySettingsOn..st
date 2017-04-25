system settings
storeNameStrategySettingsOn: aBuilder

	<systemsettings>
	(aBuilder pickOne: #storeNameStrategyClass)
		label: 'File Naming Strategy';
		description: 'Choose how will be the Epicea logs named.';
		parent: #epicea;
		target: self;
		default: OmRandomSuffixStrategy;
		domainValues: OmSessionStoreNamingStrategy availableStrategies.
