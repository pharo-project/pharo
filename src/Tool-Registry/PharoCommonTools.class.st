"
A PharoCommonTools is a ToolRegistry that knows about common tools that may be replaced by other packages. It manages a list of the recently registered tools and provides settings for reregistering to a former registered tool.

"
Class {
	#name : #PharoCommonTools,
	#superclass : #ToolRegistry,
	#instVars : [
		'recentTools'
	],
	#category : #'Tool-Registry'
}

{ #category : #settings }
PharoCommonTools class >> browsingSettingsOn: aBuilder [
	<systemsettings>
	
	"create empty #codeBrowsing and #qualityAssistant settings groups so the tools can use it"
	
	(aBuilder group: #codeBrowsing)
		label: 'Code Browsing';
		description: 'All settings concerned with code browsing look''n feel'.
	
	(aBuilder group: #qualityAssistant)
		label: 'QualityAssistant';
		parent: #codeBrowsing.
]

{ #category : #initialization }
PharoCommonTools class >> initialize [
	SessionManager default
		registerToolClassNamed: self name
]

{ #category : #settings }
PharoCommonTools class >> settingsOn: aBuilder [
	<systemsettings>
	Smalltalk tools class == self
		ifFalse: [ ^ self ].
	(aBuilder group: #pharoTools)
		parent: #pharoSystem;
		label: 'Recent Tools';
		description: 'Recently registered Tools';
		with: [ 
					(aBuilder pickOne: #browserTool)
						target: Smalltalk;
						targetSelector: #tools;
						getSelector: #browserTool;
						setSelector: #browserTool:;
						label: 'Systembrowser';
						default: (Smalltalk at: #ClyFullBrowser);
						domainValues: Smalltalk tools recentBrowserTools ];
		with: [ 
					(aBuilder pickOne: #inspectorTool)
						target: Smalltalk;
						targetSelector: #tools;
						getSelector: #inspectorTool;
						setSelector: #inspectorTool:;
						default: (Smalltalk at: #GTInspector);
						label: 'Inspector';
						domainValues: Smalltalk tools recentInspectorTools ];
		with: [ 
					(aBuilder pickOne: #workspaceTool)
						target: Smalltalk;
						targetSelector: #tools;
						getSelector: #workspaceTool;
						setSelector: #workspaceTool:;
						label: 'Workspace';
						default: (Smalltalk at: #GTPlayground);
						domainValues: Smalltalk tools recentWorkspaceTools ];
		with: [ 
					(aBuilder pickOne: #debuggerTool)
						target: Smalltalk;
						targetSelector: #tools;
						label: 'Debugger';
						domainValues: Smalltalk tools recentDebuggerTools ];
		with: [ 
					(aBuilder pickOne: #messageListTool)
						target: Smalltalk;
						targetSelector: #tools;
						label: 'Messagelist';
						default: (Smalltalk at: #ClyOldMessageBrowserAdapter);
						domainValues: Smalltalk tools recentMessageListTools ];
		with: [ 
					(aBuilder pickOne: #versionBrowserTool)
						target: Smalltalk;
						targetSelector: #tools;
						label: 'Versionbrowser';
						domainValues: Smalltalk tools recentVersionBrowserTools ];
		with: [ 
					(aBuilder pickOne: #fileListTool)
						target: Smalltalk;
						targetSelector: #tools;
						label: 'Filelist';
						default: (Smalltalk at: #FileList);
						domainValues: Smalltalk tools recentFileListTools];
		with: [ 
					(aBuilder pickOne: #changeSorterTool)
						target: Smalltalk;
						targetSelector: #tools;
						label: 'Changesorter';
						domainValues: Smalltalk tools recentChangeSorterTools]
]

{ #category : #'system startup' }
PharoCommonTools class >> shutDown: aboutToQuit [
	self allInstancesDo: #cleanUp
]

{ #category : #'world menu' }
PharoCommonTools class >> worldMenuOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'System Browser')
		parent: #Tools;
		action: [ Smalltalk tools openClassBrowser ];
		order: 0;
		keyText: 'o, b';
		help: 'System browser to browse and edit code.';
		iconName: #smallSystemBrowser
]

{ #category : #'registry access' }
PharoCommonTools >> basicInspector [

	^ self toolNamed: #basicInspector
]

{ #category : #'registry access' }
PharoCommonTools >> browser [

	^ self toolNamed: #browser
]

{ #category : #tools }
PharoCommonTools >> browserTool [
	^ self browser
]

{ #category : #tools }
PharoCommonTools >> browserTool: aTool [
	^ self register: aTool as: #browser
]

{ #category : #'registry access' }
PharoCommonTools >> changeList [

	^ self toolNamed: #changeList
]

{ #category : #'registry access' }
PharoCommonTools >> changeSorter [

	^ self toolNamed: #changeSorter
]

{ #category : #tools }
PharoCommonTools >> changeSorterTool [
	^ self changeSorter
]

{ #category : #tools }
PharoCommonTools >> changeSorterTool: aTool [
	^ self register: aTool as: #changeSorter
]

{ #category : #cleanup }
PharoCommonTools >> cleanUp [
	recentTools keysAndValuesDo:[:name :toolSet|
		toolSet copy do:[:tool | tool isObsolete ifTrue:[ toolSet remove: tool]]]
]

{ #category : #'registry access' }
PharoCommonTools >> debugger [

	^ self toolNamed: #debugger
]

{ #category : #tools }
PharoCommonTools >> debuggerTool [
	^ self debugger
]

{ #category : #tools }
PharoCommonTools >> debuggerTool: aTool [
	^ self register: aTool as: #debugger
]

{ #category : #'registry access' }
PharoCommonTools >> debuggers [

	^ self toolNamed: #debuggers
]

{ #category : #'registry access' }
PharoCommonTools >> fileList [

	^ self toolNamed: #fileList
]

{ #category : #tools }
PharoCommonTools >> fileListTool [
	^ self fileList
]

{ #category : #tools }
PharoCommonTools >> fileListTool: aTool [
	^ self register: aTool as: #fileList
]

{ #category : #'registry access' }
PharoCommonTools >> finder [

	^ self toolNamed: #finder
]

{ #category : #initialization }
PharoCommonTools >> initialize [
	recentTools := Dictionary new.
	super initialize.
	
]

{ #category : #'registry access' }
PharoCommonTools >> inspector [

	^ self toolNamed: #inspector
]

{ #category : #tools }
PharoCommonTools >> inspectorTool [
	^ self inspector
]

{ #category : #tools }
PharoCommonTools >> inspectorTool: aTool [
	^ self register: aTool as: #inspector
]

{ #category : #'registry access' }
PharoCommonTools >> messageList [

	^ self toolNamed: #messageList
]

{ #category : #tools }
PharoCommonTools >> messageListTool [
	^ self messageList
]

{ #category : #tools }
PharoCommonTools >> messageListTool: aTool [
	^ self register: aTool as: #messageList
]

{ #category : #'registry access' }
PharoCommonTools >> monticelloBrowser [

	^ self toolNamed: #monticelloBrowser
]

{ #category : #'registry access' }
PharoCommonTools >> pointerExplorer [

	^ self toolNamed: #pointerExplorer
]

{ #category : #'registry access' }
PharoCommonTools >> processBrowser [

	^ self toolNamed: #processBrowser
]

{ #category : #registration }
PharoCommonTools >> recentBrowserTools [
	^ self recentToolsFor: #browser
]

{ #category : #registration }
PharoCommonTools >> recentChangeSorterTools [
	^ self recentToolsFor: #changeSorter
]

{ #category : #registration }
PharoCommonTools >> recentDebuggerTools [
	^ self recentToolsFor: #debugger
]

{ #category : #registration }
PharoCommonTools >> recentFileListTools [
	^ self recentToolsFor: #fileList
]

{ #category : #registration }
PharoCommonTools >> recentInspectorTools [
	^ self recentToolsFor: #inspector
]

{ #category : #registration }
PharoCommonTools >> recentMessageListTools [
	^ self recentToolsFor: #messageList
]

{ #category : #registration }
PharoCommonTools >> recentToolsFor: aName [
	^ (recentTools at: aName ifAbsent: [ {} ]) reject: [ :tool | tool isObsolete ]
]

{ #category : #registration }
PharoCommonTools >> recentVersionBrowserTools [
	^ self recentToolsFor: #versionBrowser
]

{ #category : #registration }
PharoCommonTools >> recentWorkspaceTools [
	^ self recentToolsFor: #workspace
]

{ #category : #registration }
PharoCommonTools >> register: anTool as: aNameSelector [
	| usedToolSet |
	anTool ifNil:[ ^ self].
	super register: anTool as: aNameSelector.
	usedToolSet := recentTools at: aNameSelector ifAbsentPut: [ Set new ].
	usedToolSet add: anTool
]

{ #category : #'registry access' }
PharoCommonTools >> syntaxErrorDebugger [

	^ self toolNamed: #syntaxErrorDebugger
]

{ #category : #'registry access' }
PharoCommonTools >> testRunner [

	^ self toolNamed: #testRunner
]

{ #category : #'registry access' }
PharoCommonTools >> timeProfiler [

	^ self toolNamed: #timeProfiler
]

{ #category : #private }
PharoCommonTools >> toolNamed: aSymbol [

	tools at: aSymbol ifPresent: [ :tool | ^ tool ].
	
	aSymbol = #debugger ifTrue: [ 
		Smalltalk 
			logError: 'No debugger installed to handle debug request.' 
			inContext: thisContext.
		
			^ nil. "Smalltalk snapshot: false andQuit: true."
	].

	self error: 'No tool named: "' , aSymbol , '" present in the registry'.
]

{ #category : #'registry access' }
PharoCommonTools >> versionBrowser [

	^ self toolNamed: #versionBrowser
]

{ #category : #tools }
PharoCommonTools >> versionBrowserTool [
	^ self versionBrowser
]

{ #category : #tools }
PharoCommonTools >> versionBrowserTool: aTool [
	^ self register: aTool as: #versionBrowser
]

{ #category : #'registry access' }
PharoCommonTools >> workspace [

	^ self toolNamed: #workspace
]

{ #category : #tools }
PharoCommonTools >> workspaceTool [
	^ self workspace
]

{ #category : #tools }
PharoCommonTools >> workspaceTool: aTool [
	^ self register: aTool as: #workspace
]
