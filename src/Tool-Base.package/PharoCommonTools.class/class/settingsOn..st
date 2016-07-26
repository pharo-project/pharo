settings
settingsOn: aBuilder
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
						domainValues: Smalltalk tools recentBrowserTools ];
		with: [ 
					(aBuilder pickOne: #inspectorTool)
						target: Smalltalk;
						targetSelector: #tools;
						getSelector: #inspectorTool;
						setSelector: #inspectorTool:;
						label: 'Inspector';
						domainValues: Smalltalk tools recentInspectorTools ];
		with: [ 
					(aBuilder pickOne: #workspaceTool)
						target: Smalltalk;
						targetSelector: #tools;
						getSelector: #workspaceTool;
						setSelector: #workspaceTool:;
						label: 'Workspace';
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
						domainValues: Smalltalk tools recentFileListTools];
		with: [ 
					(aBuilder pickOne: #changeSorterTool)
						target: Smalltalk;
						targetSelector: #tools;
						label: 'Changesorter';
						domainValues: Smalltalk tools recentChangeSorterTools]