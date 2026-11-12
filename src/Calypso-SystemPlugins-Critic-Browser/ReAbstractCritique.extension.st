Extension { #name : 'ReAbstractCritique' }

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ReAbstractCritique class >> calypsoSystemPluginsCriticBrowserCommandsIn: aCommandMap [

	<calypsoCommands>
	aCommandMap tableIcons
		add: ClyBanCritiqueRuleCommand;
		add: ClyFixCritiqueCommand;
		add: ClyShowCritiqueDetailsCommand;
		add: ClyShowDependencyCritiqueCommand.
	aCommandMap tableDecorators add: ClyCritiquesTableDecorator
]
