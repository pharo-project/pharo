actions
ban: aCritique

	super ban: aCritique.
	
	SystemSettingsPersistence storeIdentifier: 
		ReRuleManager ruleToggleGroupID printString,
		aCritique rule class enabledSettingID printString
		"printString to ensure symbol hash"