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
WorldState >> worldMenu [
	^self menuBuilder menuEntitled: self discoveredMenuTitle
]
