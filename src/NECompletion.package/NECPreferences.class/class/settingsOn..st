settings
settingsOn: aBuilder
	<systemsettings>
	(aBuilder setting: #'Code Completion')
		target: self;
		parentName: #codeBrowsing;
		selector: #enabled;
		iconName: #smallConfigurationIcon;
		description: 'Enable or disable code completion in browsers, debuggers and workspaces.';
		with: [ 
					| availableControllers |
					availableControllers := self availableControllers.
					availableControllers size > 1
						ifTrue: [ 
							(aBuilder pickOne: #completionController)
								order: -1;
								label: 'Controller';
								getSelector: #currentController;
								setSelector: #useController:;
								domainValues: availableControllers ].
					(aBuilder setting: #backgroundColor)
						label: 'Background Color'.
					(aBuilder setting: #overrideModel)
						label: 'Complete whole methods'.	
					(aBuilder setting: #expandPrefixes)
						label: 'Complete common prefixes using TAB in the suggested completions'.
					(aBuilder setting: #useEnterToAccept)
						label: 'Use ENTER to accept a suggested completion';
						default: self defaultUseEnterToAccept.
					(aBuilder setting: #captureNavigationKeys)
						label: 'Use navigation keys for extended completion functionality'.
					(aBuilder setting: #caseSensitive)
						label: 'Case Sensitive';
						description: 'Decide if you want eCompletion to be case sensitive or not.'.
					(aBuilder setting: #smartCharacters)
						label: 'Smart Characters';
						description: 'Decide if you want eCompletion to use smart characters, e.g, to automatically close brackets.'.
					(aBuilder setting: #smartCharactersWithSingleSpace)
						label: 'Smart Characters with Single Space';
				 		description: 'Enumerate the characters which are automatically inserted with a single space in between.'.
					(aBuilder setting: #smartCharactersWithDoubleSpace) 
						label: 'Smart Characters with Double Space';
				 		description: 'Enumerate the characters which are automatically inserted with a two spaces in between.'.
					(aBuilder setting: #popupShowAutomatic)
						default: self defaultPopupShowAutomatic;
						label: 'Popup is automatic'.
					(aBuilder setting: #popupAutomaticDelay)
						default: self defaultPopupDelay;
						label: 'Popup appearance delay'.
					(aBuilder pickOne: #popupShowWithShortcut)
						default: self defaultPopupShortcut;
						label: 'Popup appears with this shortcut';
						domainValues: self availablePopupShortcuts.
					(aBuilder setting: #spaceAfterCompletion)
						default: self defaultSpaceAfterCompletion;
						label: 'Put a space after completion'
					 ].