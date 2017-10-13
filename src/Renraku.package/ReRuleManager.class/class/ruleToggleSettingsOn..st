settings
ruleToggleSettingsOn: aBuilder 
	<systemsettings>
	(aBuilder group: self ruleToggleGroupID)
		order: 1;
		label: 'Toggle rules to run';
		description: 'Select which rules do you want to see in the live feedback';
		parent: #qualityAssistant;
		with: [ 
			(self visibleRuleClasses sorted: [ :a :b | a name < b name ])
				select: [ :rule | rule inheritsFrom: ReAbstractRule ] "because lint rules don't have #enabled"
				thenDo: [ :rule | 
					| inst |
					inst := rule new.
					(aBuilder setting: rule enabledSettingID)
					selector: #enabled;
					target: rule;
					label: inst name;
					description: inst rationale ] ]