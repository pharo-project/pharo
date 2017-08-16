initialization
setUp
	super setUp.
	oldRulesSettings := Dictionary new.

	ReAbstractRule withAllSubclassesDo: [ :rule |
		oldRulesSettings at: rule put: rule enabled ]