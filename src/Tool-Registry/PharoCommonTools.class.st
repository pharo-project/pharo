"
A PharoCommonTools is a ToolRegistry that knows about common tools that may be replaced by other packages. It manages a list of the recently registered tools and provides settings for reregistering to a former registered tool.

"
Class {
	#name : 'PharoCommonTools',
	#superclass : 'ToolRegistry',
	#category : 'Tool-Registry',
	#package : 'Tool-Registry'
}

{ #category : 'settings' }
PharoCommonTools class >> browsingSettingsOn: aBuilder [
	<systemsettings>

	"create empty #codeBrowsing and #qualityAssistant settings groups so the tools can use it"

	(aBuilder group: #codeBrowsing)
		label: 'Code Browsing';
		description: 'All settings concerned with code browsing look''n feel'.

	(aBuilder group: #qualityAssistant)
		label: 'QualityAssistant';
		parent: #codeBrowsing
]

{ #category : 'class initialization' }
PharoCommonTools class >> initialize [
	SessionManager default
		registerToolClassNamed: self name
]

{ #category : 'world menu' }
PharoCommonTools class >> menuToploOn: aBuilder [
	<worldMenu>

	(aBuilder item: #LoadToplo)
		parent: #Tools;
		label: 'Load Toplo and Bloc';
		icon: (self iconNamed: #smallLoadProject);
		help: 'Load Toplo, a widget framework on top of Bloc.';
		order: 100;
		action: [
			(Smalltalk classNamed: #Metacello) new
				baseline: 'Toplo';
				repository: (Smalltalk classNamed: #BaselineOfPharo) toploRepository;
				onConflictUseIncoming;
				load ]
]

{ #category : 'system startup' }
PharoCommonTools class >> shutDown: aboutToQuit [
	self tools class cleanUp: false
]

{ #category : 'settings' }
PharoCommonTools class >> tools [

	^ Smalltalk tools
]

{ #category : 'world menu' }
PharoCommonTools class >> worldMenuOn: aBuilder [

	<worldMenu>
	
	(aBuilder item: #'System Browser')
		parent: #Browsing;
		action: [ (self tools toolNamed: #browser) open ];
		order: 0;
		keyText: 'o, b';
		help: 'System browser to browse and edit code.';
		iconName: #smallSystemBrowser.
		
	(aBuilder item: 'Git Repositories Browser')
		order: 1;
		iconName: #komitterSmalltalkhubRemote;
		parent: #Browsing;
		keyText: 'o, i';
		help:
			'Iceberg is a set of tools that allow one to handle git repositories directly from a Pharo image.';
		action: [ 
			"Pay attention using newApplication: ... will break IceTipRepositoriesBrowser because it initializes
			a model."
			((Smalltalk at: #IceTipRepositoriesBrowser) new) open ]
]
