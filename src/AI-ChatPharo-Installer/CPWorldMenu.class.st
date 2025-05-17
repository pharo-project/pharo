Class {
	#name : 'CPWorldMenu',
	#superclass : 'RSObject',
	#classInstVars : [
		'chatpharoIcon'
	],
	#category : 'AI-ChatPharo-Installer',
	#package : 'AI-ChatPharo-Installer'
}

{ #category : 'accessing' }
CPWorldMenu class >> chatpharoIcon [

	^ chatpharoIcon ifNil: [
			  chatpharoIcon := Form fromBinaryStream:
				                   self chatpharoIconStream base64Decoded
					                   readStream ]
]

{ #category : 'accessing' }
CPWorldMenu class >> chatpharoIconStream [

	^ ''
]

{ #category : 'export' }
CPWorldMenu class >> loadExporters [

	Metacello new
		githubUser: 'omarabedelkader'
		project: 'ChatPharo'
		commitish: 'main'
		path: 'src';
		baseline: 'AIChatPharo';
		load
]

{ #category : 'accessing' }
CPWorldMenu class >> loadFullVersion [
	"TODO do not discard changes"

	(UIManager default confirm:
		 'Do you want to load the ChatPharo ?') ifFalse: [
		^ self ].
	[
		Metacello new
			githubUser: 'omarabedelkader'
			project: 'ChatPharo'
			commitish: 'main'
			path: 'src';
			baseline: 'AIChatPharo';
			load ]
		on: MCMergeOrLoadWarning
		do: [ :warning | warning load ].
	self loadExporters
]

{ #category : 'accessing' }
CPWorldMenu class >> menu00ChatPharoOn: aBuilder [

	<worldMenu>
	(aBuilder item: #ChatPharo)
		parent: #Tools;
		label: 'ChatPharo';
		"icon: self chatpharoIcon;"help: 'An AI Assistant';
		order: 777.
	(aBuilder item: #ChatPharoHelp)
		parent: #ChatPharo;
		label: 'Help';
		icon: (self iconNamed: #help);
		help: 'Need help?';
		order: 1.
	(aBuilder item: #ChatPharoLoad)
		parent: #ChatPharo;
		label: 'Load...';
		icon: (self iconNamed: #smallLoadProject);
		help: 'Load ChatPharo projects';
		order: 2
]

{ #category : 'accessing' }
CPWorldMenu class >> menu04DocumentationOn: aBuilder [

	<worldMenu>
	(aBuilder item: #ChatPharoDocumentation)
		parent: #ChatPharoHelp;
		order: 4;
		label: 'Online documentation';
		help:
			'Will open the webpage https://github.com/omarabedelkader/ChatPharo/wiki';
		icon: (self iconNamed: #smallHelp);
		action: [
			WebBrowser openOn:
					'https://github.com/omarabedelkader/ChatPharo/wiki' ]
]

{ #category : 'accessing' }
CPWorldMenu class >> menu06LoadFullVersionOn: aBuilder [

	<worldMenu>
	(aBuilder item: #ChatPharoLoadFullVersion)
		parent: #ChatPharoLoad;
		order: 5;
		label: 'Load full version';
		help: 'Load full version of ChatPharo including examples';
		"icon: self chatpharoIcon;"action: [ self loadFullVersion ]
]

{ #category : 'see class side' }
CPWorldMenu >> seeClassSide [
]
