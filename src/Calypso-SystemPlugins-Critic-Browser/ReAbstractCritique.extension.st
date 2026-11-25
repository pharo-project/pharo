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

{ #category : '*Calypso-SystemPlugins-Critic-Browser' }
ReAbstractCritique class >> decorateBrowserItem: aBrowserItem by: anEnvironmentPlugin [
	| critique |
	critique := aBrowserItem actualObject.

	aBrowserItem addProperty: (ClyCritiqueSeverityProperty of: critique).
	critique sourceAnchor ifNotNil: [ :anchor |
		anchor providesInterval ifTrue: [
			aBrowserItem addProperty: (ClySourceNodeCritiqueIntervalProperty from: anchor interval)]].
	critique providesChange	ifTrue: [ aBrowserItem markWith: ClyCritiqueFixableTag ]
]
