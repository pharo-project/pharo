Extension { #name : 'WorldState' }

{ #category : '*Morphic-Desktop' }
WorldState >> discoveredMenuOn: aBuilder [

	^ aBuilder menuEntitled: self discoveredMenuTitle
]

{ #category : '*Morphic-Desktop' }
WorldState >> fallbackMenuOn: menu [
	"Build the menu that is put up if something is going wrong with the menubuilder"

	menu addTitle: 'Fallback menu'.
	menu commandKeyHandler: self.
	menu addLine.
	menu
		defaultTarget: Smalltalk tools;
		addList: #(
		('System Browser' 			#openClassBrowser)
		-
		('Transcript' 				#openTranscript)
		('File Browser'				#openFileList)
		-
		('Test Runner'				#openTestRunner)
		('Process Browser' 			#openProcessBrowser)
	).
	menu addLine.
	menu add: 'Save'
		target: self class
		selector: #saveSession.
	menu add: 'Save as...'
		target: self class
		selector: #saveAs.
	menu add: 'Save and quit'
		target: self class
		selector: #saveAndQuit.
	menu add: 'Quit'
		target: self class
		selector: #quitSession
]

{ #category : '*Morphic-Desktop' }
WorldState class >> helpOn: aBuilder [

	<worldMenu>
	(aBuilder item: #Help)
		order: 99;
		help: 'Some help tools on Pharo.';
		with: [
			(aBuilder group: #PharoHelp)
				order: 2;
				withSeparatorAfter ]
]

{ #category : '*Morphic-Desktop' }
WorldState class >> mostUsedToolsOn: aBuilder [

	(aBuilder item: #Tools)
		withSeparatorAfter;
		order: 1
]

{ #category : '*Morphic-Desktop' }
WorldState class >> pharoItemsOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Pharo)
		label: 'Pharo';
		iconName: #smallPharo;
		order: 0;
		with: [
			(aBuilder group: #Saving)
				order: 98;
				with: [
					(aBuilder item: #Save)
						target: self;
						selector: #saveSession;
						help: 'Save the current version of the image on disk.';
						order: 1;
						keyText: 'S';
						iconName: #smallSave.
					(aBuilder item: #'Save as...')
						target: aBuilder model;
						selector: #saveAs;
						help: 'Save the current version of the image on disk under a new name.';
						order: 2;
						iconName: #smallSaveAs.
					(aBuilder item: #'Save and quit')
						target: self;
						selector: #saveAndQuit;
						help: 'Save the current image on disk, and quit Pharo.';
						order: 3;
						iconName: #smallQuit.
					aBuilder	withSeparatorAfter ].
				(aBuilder item: #Quit)
					target: self;
					selector: #quitSession;
					help: 'Quit Pharo without saving the image on disk.';
					order: 99;
					iconName: #smallQuit ]
]

{ #category : '*Morphic-Desktop' }
WorldState class >> saveAndQuit [
	Cursor write showWhile: [
		Smalltalk snapshot: true andQuit: true
	]
]

{ #category : '*Morphic-Desktop' }
WorldState >> saveAs [

	| reference |
	reference := MorphicWidgetFactory new
		             chooseForSaveFileReference: 'Save Image as?'
		             extensions: #( 'image' )
		             path: Smalltalk imageFile nextVersion.
	reference ifNotNil: [ Smalltalk saveAs: reference parent / (reference basenameWithoutExtension: 'image') ]
]

{ #category : '*Morphic-Desktop' }
WorldState class >> windowsOn: aBuilder [

	<worldMenu>
	(aBuilder item: #Windows)
		order: 90;
		with: [
			(aBuilder item: #'Collapse all windows')
				action: [ self currentWorld collapseAll ];
				help: 'Reduce all open windows to collapsed forms that only show titles'.
			(aBuilder item: #'Expand all windows')
				iconName: #expandBox;
				action: [ self currentWorld expandAll ];
				help: 'Expand all collapsed windows back to their expanded forms'.
			(aBuilder item: #'Fit all windows')
				action: [ self currentWorld fitAllVisibleWindows ];
				help: 'Fit all open windows as visible in World'.
			(aBuilder item: #'Close all debuggers')
				help: 'Close all opened debugger in the image.';
				action: [ OupsDebuggerSystem closeAllDebuggers ].
			(aBuilder item: #'Send top window to back (\)')
				action: [ SystemWindow sendTopWindowToBack ];
				help: 'Make the topmost window become the backmost one, and activate the window just beneath it.'.
			(aBuilder item: #'Delete unchanged windows')
				action: [ self currentWorld closeUnchangedWindows ];
				help: 'Deletes all windows that do not have unsaved text edits.'.
			(aBuilder item: #'Delete all windows discarding edits')
				action: [ self currentWorld closeAllWindowsDiscardingChanges ];
				help: 'Deletes all windows even if they have unsaved text edits.'.
			(aBuilder item: #'Toggle full screen mode')
				action: [ WorldMorph toggleFullscreen ];
				help: 'Turn full screen mode on when it is off, off when it is on.' ]
]

{ #category : '*Morphic-Desktop' }
WorldState >> worldMenu [
	^self menuBuilder menuEntitled: self discoveredMenuTitle
]
