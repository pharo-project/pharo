settings
settingsOn: aBuilder 
	<systemsettings>
	(aBuilder group: #Iceberg)
		parent: #SCM;
		with: [ 
			(aBuilder setting: #enableMetacelloIntegration)
				target: self;
				label: 'Enable Metacello integration';
				description: 'If selected, Metacello github// repositories will be loaded using iceberg';
"				icon: (Smalltalk ui icons iconNamed: #smallConfigurationIcon);"
				with: [ 
					(aBuilder pickOne: #remoteTypeSelector)
						label: 'Remote type';
						description: 'When creating an Iceberg repository out of a github:// URL choose whether you want to user an SCP URL (i.e. with the form ''git@github.com:<username>/<project>.git'') or an HTTPS URL (i.e. with the form ''https://github.com/<username>/<project>.git''';
						domainValues: {
							'SCP (git@github.com:<username>/<project>.git)' -> #scpUrl.
							'HTTPS (https://github.com/<username>/<project>.git)' -> #httpsUrl
						}.
				]
		]