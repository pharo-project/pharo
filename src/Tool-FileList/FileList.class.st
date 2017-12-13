"
A FileList is a tool to deal with files.

FileList open				""open FileList""
"
Class {
	#name : #FileList,
	#superclass : #Model,
	#instVars : [
		'reference',
		'volumeList',
		'volumeListIndex',
		'list',
		'listIndex',
		'pattern',
		'brevityState',
		'dirSelectionBlock',
		'modalView',
		'ok',
		'contents',
		'optionalButtonSpecs',
		'grid',
		'fileEncoding',
		'sortBlock',
		'baseLabel',
		'configuredServices',
		'sourceTextModel'
	],
	#classVars : [
		'FileReaderRegistry'
	],
	#classInstVars : [
		'searchList'
	],
	#category : #'Tool-FileList'
}

{ #category : #utility }
FileList class >> addFullPanesTo: window from: aCollection [

	aCollection do: [ :each | 
		window addMorph: each first fullFrame: each second ]
]

{ #category : #menu }
FileList class >> contentMenu: aBuilder [
	<contextMenu>
	<fileListContentMenu>
	
	(aBuilder item: #'Find...' translated)
		keyText: 'f';
		selector: #find;
		iconName: #smallFindIcon.
	(aBuilder item: #'Find again' translated)
		keyText: 'g';
		selector: #findAgain;
		iconName: #smallFindIcon.
	(aBuilder item: #'Set search string' translated)
		keyText: 'h';
		selector: #setSearchString;
		withSeparatorAfter.
		
	(aBuilder item: #'Do again' translated)
		keyText: 'j';
		selector: #again;
		iconName: #smallRedoIcon.
	(aBuilder item: #'Undo' translated)
		keyText: 'z';
		selector: #undo;
		iconName: #smallUndoIcon;
		withSeparatorAfter.
		
	(aBuilder item: #'Copy' translated)
		keyText: 'c';
		selector: #copySelection;
		iconName: #smallCopyIcon.
	(aBuilder item: #'Cut' translated)
		keyText: 'x';
		selector: #cut;
		iconName: #smallCutIcon.
	(aBuilder item: #'Paste' translated)
		keyText: 'v';
		selector: #paste;
		iconName: #smallPasteIcon.
	(aBuilder item: #'Paste...' translated)
		selector: #pasteRecent;
		iconName: #smallPasteIcon;
		withSeparatorAfter .
		
	(aBuilder item: #'Do it' translated)
		keyText: 'd';
		selector: #doIt;
		iconName: #smallDoItIcon.
	(aBuilder item: #'Print it' translated)
		keyText: 'p';
		selector: #printIt;
		iconName: #smallPrintItIcon.
	(aBuilder item: #'Inspect it' translated)
		keyText: 'i';
		selector: #inspectIt;
		iconName: #smallInspectItIcon.
	(aBuilder item: #'FileIn selection' translated)
		keyText: 'G';
		selector: #fileItIn;
		withSeparatorAfter.
		
	(aBuilder item: #'Accept' translated)
		keyText: 's';
		selector: #accept;
		iconName: #smallOkIcon.
	(aBuilder item: #'Cancel' translated)
		keyText: 'l';
		selector: #cancel;
		iconName: #smallCancelIcon;
		withSeparatorAfter.

]

{ #category : #utility }
FileList class >> itemsForDirectory: aFileDirectory [
	"Answer a list of services appropriate when no file is selected."

	^ FileServices itemsForDirectory: aFileDirectory
]

{ #category : #utility }
FileList class >> itemsForFile: file [
	"Answer a list of services appropriate for a file of the given full name"

	^ FileServices itemsForFile: file fullName
]

{ #category : #'world menu' }
FileList class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #'File Browser')
		parent: #Tools;
		order: 0.43;
		action: [ self open ];
		help: 'Browse the files present on your system.';
		icon: self taskbarIcon.
	aBuilder withSeparatorAfter
]

{ #category : #'morphic ui' }
FileList class >> morphicViewOnDirectory: aFileDirectory [

	^ self 
		morphicViewOnFile: aFileDirectory 
		contents: nil
		fileList: (self new directory: aFileDirectory).
]

{ #category : #'morphic ui' }
FileList class >> morphicViewOnFile: aFile contents: contents fileList: aFileList [
	| window fileListBottom midLine fileListTopOffset buttonPane fileContentsView |
 	window := (SystemWindow labelled: aFile fullName) model: aFileList.
	fileListTopOffset := TextStyle defaultFont pointSize * 2 + 11.
	fileListBottom := 0.4.
	midLine := 0.2.
	buttonPane := aFileList optionalButtonRow addMorph: aFileList morphicPatternPane.
	self
		addFullPanesTo: window
		from:
			{{aFileList morphicDirectoryTreePane.
			(LayoutFrame identity rightFraction: midLine)}.
			{buttonPane.
			(LayoutFrame identity leftFraction: midLine; bottomFraction: 0;  bottomOffset: fileListTopOffset - 3)}.
			
			{aFileList morphicFileListPane.
			(LayoutFrame identity leftFraction: midLine; bottomFraction: fileListBottom; topOffset: fileListTopOffset)}.
			
			{(fileContentsView := aFileList morphicFileContentsPane).
			(LayoutFrame identity leftFraction: midLine; topFraction: fileListBottom)}}.
	contents
		ifNotNil: [ fileContentsView editString: contents.
			fileContentsView hasUnacceptedEdits: true ].
	aFileList postOpen.
	^ window
]

{ #category : #'instance creation' }
FileList class >> newOnDirectory: aFileReference [
	
	^ self newOnDirectory: aFileReference baseLabel: ''
]

{ #category : #'instance creation' }
FileList class >> newOnDirectory: aFileReference baseLabel: aTitle [
	
	^ self newOnDirectory: aFileReference baseLabel: aTitle contents: nil
]

{ #category : #'instance creation' }
FileList class >> newOnDirectory: aFileReference baseLabel: aTitle contents: someContents [
	
	^ self new
			directory: aFileReference;
			baseLabel: aTitle;
			contents: someContents;
			yourself
]

{ #category : #'morphic ui' }
FileList class >> open [
	"Open a view of an instance of me on the default directory."
	"FileList open openInWorld"
	^ self new morphicView openInWorld
]

{ #category : #'morphic ui' }
FileList class >> openEditorOn: aFileStream editString: editString [ 
	"Open an editor on the given FileStream."
	^ (self openMorphOn: aFileStream editString: editString) openInWorld
]

{ #category : #'morphic ui' }
FileList class >> openMorphOn: aFileStream editString: editString [ 
	"Open a morphic view of a FileList on the given file."
	^ (self new setFileStream: aFileStream)
		contents: editString;
		morphicViewOnFile: aFileStream fullName asFileReference
]

{ #category : #'morphic ui' }
FileList class >> openOn: aFileReference [
	^self openOn: aFileReference titled: '' 
]

{ #category : #'morphic ui' }
FileList class >> openOn: aFileReference titled: aTitle [
	
	| listView |
	listView := self newOnDirectory: aFileReference baseLabel: aTitle.
	listView morphicView openInWorld.
	^ listView
]

{ #category : #'tools registry' }
FileList class >> registerToolsOn: registry [
	"Add ourselves to registry. See [Smalltalk tools]" 
	registry register: self as: #fileList

]

{ #category : #accessing }
FileList class >> searchList [
	^ searchList ifNil: [ searchList := OrderedCollection new ].
]

{ #category : #utility }
FileList class >> suffixOf: aName [
	"Answer the file extension of the given file"
	^ aName
		ifNil: ['']
		ifNotNil: [ aName asFileReference extension asLowercase]
]

{ #category : #icons }
FileList class >> taskbarIconName [
	"Answer the icon for the receiver in a task bar."

	^#smallOpenIcon
]

{ #category : #accessing }
FileList class >> theme [
	^ Smalltalk ui theme 
]

{ #category : #'drag\'n\'drop' }
FileList >> acceptDroppingMorph: aTransferMorph event: evt inMorph: dest [
	| oldEntry destDirectory newEntry response |
	destDirectory := self dropDestinationDirectory: dest event: evt.
	oldEntry := FileSystem disk referenceTo: aTransferMorph passenger.
	newEntry := destDirectory / oldEntry basename.
	oldEntry = newEntry ifTrue: [ "Transcript nextPutAll: 'same as old name'; cr." ^ true ].
	newEntry ifNotNil: [ | msg |
		msg := String streamContents: [ :s |
			s nextPutAll: 'destination file ';
				nextPutAll: newEntry parent fullName;
				nextPutAll: ' exists already,';
				cr;
				nextPutAll: 'and is ';
				nextPutAll: (oldEntry modificationTime < newEntry modificationTime
					ifTrue: [ 'newer' ] ifFalse: [ 'not newer' ]);
				nextPutAll: ' than source file ';
				nextPutAll: oldEntry parent fullName;
				nextPut: $.;
				cr;
				nextPutAll: 'Overwrite file ';
				nextPutAll: newEntry basename;
				nextPut: $?
		].
		response := self confirm: msg.
		response ifFalse: [ ^false ].
	].

	aTransferMorph shouldCopy
		ifTrue: [ oldEntry copyAs: newEntry ]
		ifFalse: [ oldEntry renameTo: newEntry ].

	self updateFileList; fileListIndex: 0.

	aTransferMorph source model ~= self
		ifTrue: [ aTransferMorph source model updateFileList; fileListIndex: 0 ].
	"Transcript nextPutAll: 'copied'; cr."
	^true
]

{ #category : #utility }
FileList >> addFullPanesTo: window from: aCollection [

	aCollection do: [ :each | 
		window addMorph: each first fullFrame: each second ]
]

{ #category : #'file menu action' }
FileList >> addNew: aString byEvaluating: aBlock [
	"A parameterization of earlier versions of #addNewDirectory and
	#addNewFile.  Fixes the bug in each that pushing the cancel button
	in the FillInTheBlank dialog gave a walkback."

	| response newName index |
	self okToChange ifFalse: [^ self].
	(response := UIManager default
						request: ('New {1} Name?' translated format: {aString translated})
						initialAnswer: ('{1}Name' translated format: {aString translated}))
		isEmptyOrNil ifTrue: [^ self].
	newName := response asFileName.
	Cursor wait showWhile: [
		aBlock value: newName].
	self updateFileList.
	self updateDirectory.
	list ifNotNil:[
	index := list indexOf: newName.
	index = 0 ifTrue: [
		index := list findFirst: [:line | line basename endsWith: newName]].
	self fileListIndex: index.]

]

{ #category : #'own services' }
FileList >> addNewDirectory [
	self 
		addNew: 'Directory'
		byEvaluating: [:newName | 
			(self directory / newName ) ensureCreateDirectory ]

]

{ #category : #'file menu action' }
FileList >> addNewFile [
	self 
		addNew: 'File'
		byEvaluating: [:newName | (self directory / newName) ensureCreateFile ]

]

{ #category : #private }
FileList >> addPath: aString [
	"Add the given string to the list of recently visited directories."

	| full |
	aString ifNil: [^self].
	full := String streamContents: [ :strm | 
		2 to: volumeList size do: [ :i | 
			strm nextPutAll: (volumeList at: i) trimBoth.
			strm nextPut: FileSystem disk separator]].
	full := full, aString.
]

{ #category : #accessing }
FileList >> baseLabel: aString [ 

	baseLabel := aString
]

{ #category : #'own services' }
FileList >> basicDeleteDirectory [
	"Remove the currently selected directory"
	reference entries isEmpty
		 ifFalse:[^self inform:'Directory must be empty'].
	
	(self confirm: 'Really delete ' , reference basename , '?') 
		ifFalse: [^ self].
	
	self volumeListIndex: self volumeListIndex-1.

	reference delete.
	self directory: reference parent
]

{ #category : #initialization }
FileList >> buttonSelectorsToSuppress [
	"Answer a list of action selectors whose corresponding services we would prefer *not* to have appear in the filelist's button pane; this can be hand-jimmied to suit personal taste."

	^ #(removeLineFeeds: addFileToNewZip: compressFile:)
]

{ #category : #private }
FileList >> cancelHit [

	modalView delete.
]

{ #category : #'volume list and pattern' }
FileList >> changeDirectoryTo: aFileDirectory [
	"Change directory as requested."
	self directory: aFileDirectory.
	self updateDirectory
]

{ #category : #updating }
FileList >> changedContents [

	self sourceTextModel setText: self contents.
]

{ #category : #accessing }
FileList >> configuredServices [

	^ configuredServices
]

{ #category : #private }
FileList >> confirmOverwriteOrCreateNewFileOrAbort [
	| choice  chooseList |
	chooseList := {'overwrite that file' translated.
	               'choose another name' translated . 
	               'cancel' translated. }.
	choice := UIManager default
		chooseFrom: chooseList
		title: 'Save contents' translated.
	^ choice
]

{ #category : #accessing }
FileList >> contents [
	"Answer the contents of the file, reading it first if needed."
	"Possible brevityState values:
		FileList,
		fullFile, briefFile, needToGetFull, needToGetBrief,
		fullHex, briefHex, needToGetFullHex, needToGetBriefHex"
	self reference isReadable 
		ifFalse: [ ^ 'cannot read {1}' translated format: { reference }].
	(listIndex = 0) | (brevityState == #FileList) ifTrue: [^ self defaultContents].  "no file selected"
	brevityState == #fullFile ifTrue: [^ contents]. 
	brevityState == #fullHex ifTrue: [^ contents].
	brevityState == #briefFile ifTrue: [^ contents].
	brevityState == #briefHex ifTrue: [^ contents].

	brevityState == #needToGetFullHex ifTrue: [^ self readContentsHex: false].
	brevityState == #needToGetBriefHex ifTrue: [^ self readContentsHex: true].

	brevityState == #needToGetFull ifTrue: [^ self readContentsBrief: false].
	brevityState == #needToGetBrief ifTrue: [^ self readContentsBrief: true].  "default"
	
	self error: 'unknown state ' , brevityState printString
]

{ #category : #accessing }
FileList >> contents: someContents [
	
	contents := someContents
]

{ #category : #'menu messages' }
FileList >> copyName [

	listIndex = 0 ifTrue: [^ self].
	Clipboard clipboardText: self fullName asText.

]

{ #category : #private }
FileList >> defaultContents [
	contents := list isNil
		ifTrue: [String new]
		ifFalse: [
			String streamContents: [:s | 
				s << 'NO FILE SELECTED' translated; cr.
				s << '  -- Folder Summary --' translated; cr.
				list do: [:item | s << item basename; cr]]].
	brevityState := #FileList.
	^ contents
]

{ #category : #'own services' }
FileList >> deleteDirectory [
	self basicDeleteDirectory.
	self updateFileList.
	self updateDirectory.

]

{ #category : #'file menu action' }
FileList >> deleteFile [
	"Delete the currently selected file"
	listIndex = 0 ifTrue: [^ self].
	
	(self confirm: ('Really delete {1}?' translated format: { reference basename } ))
		ifFalse: [^ self].
	
	reference delete.
	reference := reference parent.
	
	self updateFileList.
	brevityState := #FileList.
	self get
]

{ #category : #private }
FileList >> directoriesFor: directory [
	"item may be file directory or server directory"
	| entries |
	entries := directory directories.
	dirSelectionBlock ifNotNil:[ entries := entries select: dirSelectionBlock ].
	^entries
]

{ #category : #accessing }
FileList >> directory [
	
	^ reference isFile 
		ifTrue: [ reference parent ]
		ifFalse: [ reference ]
]

{ #category : #accessing }
FileList >> directory: dir [
	^ self reference: dir
]

{ #category : #accessing }
FileList >> doItContext [
	^ nil
]

{ #category : #accessing }
FileList >> doItReceiver [
	^ nil
]

{ #category : #'drag\'n\'drop' }
FileList >> dragPassengerFor: item inMorph: dragSource [
	^ item contents copy

]

{ #category : #'drag\'n\'drop' }
FileList >> dragTransferTypeForMorph: aMorph [
	^#file
]

{ #category : #'drag\'n\'drop' }
FileList >> dropDestinationDirectory: dest event: evt [
	"Answer a FileDirectory representing the drop destination in the directory hierarchy morph dest"
	^ (dest itemFromPoint: evt position) withoutListWrapper
]

{ #category : #initialization }
FileList >> dynamicButtonServices [
	"Answer services for buttons that may come and go in the button pane, depending on selection"

	^ reference isDirectory 
		ifTrue: [#()]
		ifFalse:
			[ | toReject |
				toReject := self buttonSelectorsToSuppress.
				(self itemsForFile: reference) reject:
					[:svc | toReject includes: svc selector]]
]

{ #category : #'file list menu' }
FileList >> fileContentsMenu: aMenu shifted: shifted [
	"Construct aMenu to have items appropriate for the file browser's code pane, given the shift state provided"

	| shiftMenu services |
	shifted ifTrue:
		[shiftMenu := SmalltalkEditor shiftedYellowButtonMenu.
		^ aMenu addAllFrom: shiftMenu].
	reference ifNotNil:
		[services := OrderedCollection new.
		(#(briefHex briefFile needToGetBriefHex needToGetBrief) includes: brevityState) ifTrue:
			[services add: self serviceGet].
		(#(fullHex briefHex needToGetFullHex needToGetBriefHex) includes: brevityState) ifFalse:
			[services add: self serviceGetHex].
		(#(needToGetShiftJIS needToGetEUCJP needToGetCNGB needToGetEUCKR needToGetUTF8) includes: brevityState) ifFalse:
			[services add: self serviceGetEncodedText].
		(FileStream sourceFileSuffixes includes: self suffixOfSelectedFile) ifTrue:
			[services addAll: (self itemsForFile: reference)].

		aMenu 
			addServices: services
			for: self reference
			extraLines: #()].

	aMenu addAllFromPragma: 'fileListContentMenu' target: self.

	^ aMenu

]

{ #category : #private }
FileList >> fileEncoding: aByteSymbol [ 
	fileEncoding := aByteSymbol.
	brevityState := #needToGetBrief.
	self changedContents
]

{ #category : #accessing }
FileList >> fileList [
	"Answer the list of files in the current volume."

	^ list
]

{ #category : #accessing }
FileList >> fileListIndex [
	"Answer the index of the currently selected file."
	^ listIndex
]

{ #category : #accessing }
FileList >> fileListIndex: anInteger [
	"Select the file name having the given index, and display its contents."

	| item |
	self okToChange ifFalse: [^ self].
	listIndex := anInteger.
	listIndex = 0 
		ifFalse: [
			item := (list at: anInteger).
			item isDirectory
				ifTrue: [
					listIndex := 0.
					brevityState := #FileList.
					self addPath: item fullName.
					self directory: item ]
				ifFalse: [reference := item]].  "open the file selected"

	brevityState := #needToGetBrief.
	self changed: #fileListIndex.
	self changedContents.
	self updateButtonRow
]

{ #category : #'file list menu' }
FileList >> fileListMenu: aMenu [

	reference
		ifNil: [^ self noFileSelectedMenu: aMenu]
		ifNotNil: [^ self fileSelectedMenu: aMenu].

]

{ #category : #'file list menu' }
FileList >> fileSelectedMenu: aMenu [
	| n1 n2 n3 services |
	services := OrderedCollection withAll: (self itemsForFile: reference).
	n1 := services size.
	services addAll: self itemsForAnyFile.
	n2 := services size.
	services addAll: self itemsForNoFile.
	n3 := services size.
	services := services collect: [ :svc | svc copy.].
	services do: [ :svc | svc addDependent: self; doNotUseLineAfter ].
	^ aMenu 
		addServices: services 
		for: self
		extraLines: (Array with: n1 with: n2 with: n3)

]

{ #category : #accessing }
FileList >> fileSystem [
	^ reference 
		ifNil: [ FileSystem disk ]
		ifNotNil: [ reference fileSystem ]
]

{ #category : #private }
FileList >> filesMatching: aPattern [
	"Answer a list of directory entries which match the patternString.
	The patternString may consist of multiple patterns separated by ';'.
	Each pattern can include a '*' or '#' as wildcards - see String>>match:"

	| files aRegex |
	files :=  [self directory files] on:DirectoryDoesNotExist do:[#()].
	aPattern ifNil:[^files].

	aRegex := aPattern asRegex.
	^ files select: [:entry | (aRegex search: entry basename)]
]

{ #category : #private }
FileList >> findFileListIndex: aMorphTreeSelectionChanged [ 
	| node selectedPath |
	
	selectedPath := aMorphTreeSelectionChanged selection selectedNodePath.
	selectedPath ifNil: [^ self].
	node := selectedPath first.
	self fileListIndex: (self fileList indexOf: node item).

]

{ #category : #initialization }
FileList >> folderSelectButtonRow [
	"Answer the button row used for folder selection"

	| aRow |
	aRow := AlignmentMorph newRow beSticky.
	aRow color: Color transparent.
	aRow clipSubmorphs: true.
	aRow layoutInset: 0@0; cellInset: 6.
	aRow setNameTo: 'buttons'.
	^ aRow
]

{ #category : #private }
FileList >> fullName [
	"Answer the full name for the currently selected file; answer nil if no file is selected."
	^ reference fullName
]

{ #category : #'file menu action' }
FileList >> get [
	"Get contents of file again, it may have changed. Do this by making the cancel string be the contents, and doing a cancel."

	Cursor read
		showWhile: [ 
			self okToChange
				ifFalse: [ ^ nil ].
			brevityState := brevityState == #briefHex
				ifTrue: [ #needToGetFullHex ]
				ifFalse: [ #needToGetFull ].
			self changedContents
			 ]
]

{ #category : #'file menu action' }
FileList >> getEncodedText [

	Cursor read showWhile: [
		self selectEncoding.
		self changed: #contents].

]

{ #category : #accessing }
FileList >> getEncoding [
	^ fileEncoding
		ifNil: [ 'utf-8' ]
		
]

{ #category : #'file menu action' }
FileList >> getHex [
	"Get contents of file again, and display in Hex. Do this by making the cancel string be the contents, and doing a cancel."

	Cursor read showWhile: [
		brevityState := #needToGetBriefHex.
		self changedContents].

]

{ #category : #accessing }
FileList >> getListIndex [
	^ list indexOf: reference ifAbsent: [ 0 ]
]

{ #category : #private }
FileList >> getSelectedFolder [
	"Answer the selected folder or nil if dialog was canceled"

	ok ifFalse: [ ^ nil ].
	^ self directory
]

{ #category : #'user interface' }
FileList >> grid [
	^ grid ifNil: [ grid := self morphicGrid ]
]

{ #category : #initialization }
FileList >> initialDirectoryList [

	| dirList |
	dirList := self fileSystem root directories collect: [ :each |
		FileDirectoryWrapper with: each name: each basename model: self].
	
	dirList isEmpty ifTrue:[
		dirList := Array with: (FileDirectoryWrapper 
			with: self fileSystem workingDirectory
			name: self fileSystem workingDirectory basename 
			model: self)].
		
	^dirList
]

{ #category : #initialization }
FileList >> initialize [

	super initialize.
	ok := false.
	sourceTextModel := RubScrolledTextModel new interactionModel: self.
	dirSelectionBlock := [ :dirName | true].
	baseLabel := ''.
	configuredServices := OrderedCollection new.
	self directory: FileSystem workingDirectory.
]

{ #category : #'drag\'n\'drop' }
FileList >> isDirectoryList: aMorph [
	^aMorph isKindOf: SimpleHierarchicalListMorph
]

{ #category : #private }
FileList >> isFileSelected [
	"return if a file is currently selected"

	^ reference notNil and: [ reference isFile ].
]

{ #category : #'file list menu' }
FileList >> itemsForAnyFile [
	"Answer a list of universal services that could apply to any file"
	
	| services |
	services := OrderedCollection new: 4.
	services add: self serviceCopyName. 
	services add: self serviceRenameFile. 
	services add: self serviceDeleteFile.
	services add: self serviceViewContents.
	services add: self serviceViewContentsInWorkspace.
	^ services
]

{ #category : #'file list menu' }
FileList >> itemsForDirectory: dir [ 
	| services |
	services := OrderedCollection new.
	dir ifNotNil: [
		services
			addAll: (self class itemsForDirectory: dir).
		services ifNotEmpty: [services last useLineAfter] ].
	services add: self serviceAddNewFile.
	services add: self serviceAddNewDirectory.
	services addAll: self configuredServices.
	^ services
]

{ #category : #'file list menu' }
FileList >> itemsForFile: file [
	"Answer a list of services appropriate for a file"
	^ (self class itemsForFile: file).
]

{ #category : #'file list menu' }
FileList >> itemsForNoFile [

	| services |
	services := OrderedCollection new.
	services addAll: (self itemsForDirectory: (self isFileSelected ifFalse: [ self directory ] ifTrue: [])).
	^ services

		
]

{ #category : #private }
FileList >> knownEncodingIdentifiers [
	^ ZnCharacterEncoder knownEncodingIdentifiers sort
]

{ #category : #initialization }
FileList >> labelString [
	reference ifNil: [ ^ '[]' ].
	^ baseLabel, reference basename contractTo: 50
]

{ #category : #'volume list and pattern' }
FileList >> listForPattern: aPattern [
	"Make the list be those file names which match the pattern."

	| newList |
	newList := self filesMatching: aPattern.
	newList := newList asArray sort: self sortBlock.
	^ newList 
]

{ #category : #private }
FileList >> modalView: aSystemWindowOrSuch [

	modalView := aSystemWindowOrSuch
]

{ #category : #'user interface' }
FileList >> morphicDirectoryTreePane [

	^self morphicDirectoryTreePaneFiltered: #initialDirectoryList

]

{ #category : #'user interface' }
FileList >> morphicDirectoryTreePaneFiltered: aSymbol [
	^(SimpleHierarchicalListMorph 
		on: self
		list: aSymbol
		selected: #selectedDirectory
		changeSelected: #selectedDirectory:
		menu: #volumeMenu:
		keystroke: nil)
			autoDeselect: false;
			enableDrag: false;
			enableDrop: true;
			yourself
		

]

{ #category : #'user interface' }
FileList >> morphicFileContentsPane [
	| scrolledText |
	scrolledText := self sourceTextModel newScrolledText
		beWrapped;
		beForSmalltalkComment;
		yourself.
	scrolledText menuProvider: self selector: #fileContentsMenu:shifted:.
	scrolledText announcer when: RubTextAccepted send: #whenTextAccepted: to: self.
	self sourceTextModel setText: self contents.
	^ scrolledText
]

{ #category : #'user interface' }
FileList >> morphicFileListPane [
	^ grid ifNil: [ grid := self morphicGrid ].

]

{ #category : #'user interface' }
FileList >> morphicGrid [
	grid := FileListGrid new
		parent: self.
	grid
		onSelectionChangeSend: #findFileListIndex: to: self ;
		menu: #fileListMenu  shifted: false.
	^ grid treeMorph

]

{ #category : #'user interface' }
FileList >> morphicPatternPane [
	"Remove the vertical scrollbar since the minHeight would otherwise
	be too large to fit the layout frame. Added here for Pharo since
	FileList2 has been merged into FileList."
	
	^ SearchMorph new
		model: self;
		setIndexSelector: #pattern:;
		getIndexSelector: #pattern;		
		searchList: self class searchList;
		vResizing: #spaceFill;
		hResizing: #spaceFill;
		yourself
]

{ #category : #'morphic ui' }
FileList >> morphicView [

	^ self morphicViewOnDirectory: self directory
]

{ #category : #'modal dialogs' }
FileList >> morphicViewFolderSelector: aDir [
	"Answer a tool that allows the user to select a folder"
	|  window buttonRow fileListTopOffset |
	self directory: aDir.
	self optionalButtonSpecs: self servicesForFolderSelector.
	window := (SystemWindow labelled: aDir fullName) model: self.
	self modalView: window.
	buttonRow := self folderSelectButtonRow.
	self universalButtonServices do: 
			[:service |
				buttonRow addMorphBack: (service buttonToTriggerIn: self)].
	fileListTopOffset := (TextStyle defaultFont pointSize * 2) + 11.
	
	self addFullPanesTo: window from: {
		{self morphicDirectoryTreePane.  (LayoutFrame identity topOffset: fileListTopOffset) }.
		{buttonRow  . (LayoutFrame identity bottomFraction: 0; bottomOffset: fileListTopOffset-3)}.
	}.
	
	self postOpen.
	^ window 
]

{ #category : #'morphic ui' }
FileList >> morphicViewOnDirectory: aFileDirectory [

	^ self morphicViewOnDirectory: aFileDirectory baseLabel: ''
]

{ #category : #'morphic ui' }
FileList >> morphicViewOnDirectory: aFileDirectory baseLabel: aString [

	^ self
		contents: nil;
		morphicViewOnFile: aFileDirectory
]

{ #category : #'morphic ui' }
FileList >> morphicViewOnFile: aFile [
	| window fileListBottom midLine fileListTopOffset buttonPane fileContentsView |
	window := (SystemWindow labelled: self labelString) model: self.
	fileListTopOffset := TextStyle defaultFont pointSize * 2 + 11.
	fileListBottom := 0.4.
	midLine := 0.2.
	buttonPane := self optionalButtonRow addMorph: self morphicPatternPane.
	self
		addFullPanesTo: window
		from:
			{{self morphicDirectoryTreePane.
			(LayoutFrame identity rightFraction: midLine )}.
			{buttonPane.
			(LayoutFrame identity leftFraction: midLine ; bottomFraction: 0 ; bottomOffset: fileListTopOffset - 3)}.
			{self morphicFileListPane.
			(LayoutFrame identity leftFraction: midLine ; bottomFraction: fileListBottom ; topOffset: fileListTopOffset)}.
			{(fileContentsView := self morphicFileContentsPane).
			(LayoutFrame identity leftFraction: midLine ; topFraction: fileListBottom)}.
			}.
	contents
		ifNotNil: [ fileContentsView setText: contents asText.
			fileContentsView hasUnacceptedEdits: true ].
	self postOpen.
	^ window
]

{ #category : #'file list menu' }
FileList >> noFileSelectedMenu: aMenu [

	^ aMenu
		addServices: self itemsForNoFile 
		for: self
		extraLines: #()
		

]

{ #category : #private }
FileList >> okHit [
	ok := true.
	self directory
		ifNil: [self inform: 'Missing directory']
		ifNotNil: [modalView delete]
]

{ #category : #'own services' }
FileList >> okayAndCancelServices [
	"Answer ok and cancel services"

	^ {self serviceOkay. self serviceCancel}
]

{ #category : #'own services' }
FileList >> openImageInWindow [
	"Handle five file formats: GIF, JPG, PNG, Form stoteOn: (run coded), and
	BMP. Fail if file format is not recognized."
	| image |
	self reference streamWritable: false do: [ :stream|
		image := Form fromBinaryStream: stream].
	
	(World drawingClass withForm: image) openInWorld
]

{ #category : #initialization }
FileList >> optionalButtonRow [
	"Answer the button row associated with a file list"

	| aRow |
	aRow := AlignmentMorph newRow beSticky.
	aRow color: Color transparent.
	aRow clipSubmorphs: true.
	aRow layoutInset: 0@0; cellInset: 6.
	aRow setNameTo: 'buttons'.
	aRow setProperty: #buttonRow toValue: true.  "Used for dynamic retrieval later on"
	^ aRow
]

{ #category : #'user interface' }
FileList >> optionalButtonSpecs [
	^ optionalButtonSpecs
]

{ #category : #'user interface' }
FileList >> optionalButtonSpecs: aSpecArray [
	optionalButtonSpecs := aSpecArray
]

{ #category : #accessing }
FileList >> pattern [
	^ pattern 

]

{ #category : #accessing }
FileList >> pattern: aStringOrNil [
	pattern := aStringOrNil
		ifNotNil: [ :aString | 
			aString asString trimBoth
				ifEmpty: [ nil ]
				ifNotEmpty: [ :trimmed | trimmed ] ].
	self updateFileList
]

{ #category : #'menu messages' }
FileList >> perform: selector orSendTo: otherTarget [ 
	"Selector was just chosen from a menu by a user.
	If it's one of the three sort-by items, handle it specially.
	If I can respond myself, then perform it on myself. 
	If not, send it to otherTarget, presumably the editPane from which the menu was invoked."
 
	^ (#(get getHex copyName openImageInWindow renameFile deleteFile deleteDirectory addNewFile ) includes: selector)
				ifTrue: [self perform: selector]
				ifFalse: [super perform: selector orSendTo: otherTarget]
]

{ #category : #private }
FileList >> postOpen [

	reference ifNotNil: [
		self changed: #(openPath) , reference pathSegments. 
	].

]

{ #category : #private }
FileList >> put: aText [
	"Private - put the supplied text onto the file"

	|  type |
	brevityState == #fullFile 
		ifTrue:[
			Cursor write showWhile: [reference writeStream truncate;nextPutAll: aText asString; close].
			^ true].

	listIndex = 0 
		ifTrue: [
			self inform: 'No fileName is selected' translated.
			^ false  "failed" ].
	type := 'These'.
	
	brevityState = #briefFile ifTrue: [type := 'Abbreviated'].
	brevityState = #briefHex ifTrue: [type := 'Abbreviated'].
	brevityState = #fullHex ifTrue: [type := 'Hexadecimal'].
	brevityState = #FileList ifTrue: [type := 'Directory'].
	
	self inform: ('{1} contents cannot
meaningfully be saved at present.' translated format:{type translated}).
	^ false  "failed"
]

{ #category : #private }
FileList >> readContentsBrief: brevityFlag [
	"Read the contents of the receiver's selected file, unless it is too long, in which case show just the first 5000 characters. Don't create a file if it doesn't already exist."
	| fileSize first5000 |
	self reference binaryReadStreamDo: [:stream |	
		(brevityFlag not or: [(fileSize := stream size) <= 100000]) 
			ifTrue:[
				contents :=   self streamFullContentsFrom: stream.
				brevityState := #fullFile.   "don't change till actually read"
				^ contents].

		"if brevityFlag is true, don't display long files when first selected"
		 first5000 := self streamFirst5000BytesFrom: stream].
	
	contents := '{1}
------------------------------------------
... end of the first 5000 characters.' translated format: {first5000}.
	brevityState := #briefFile.   "don't change till actually read"
	^ contents.

]

{ #category : #private }
FileList >> readContentsHex: brevity [
	"retrieve the contents from the external file unless it is too long.
	  Don't create a file here.  Check if exists."
	| size data hexData s |

	self reference streamWritable: false do: [ :f| 
		f isNil 
			ifTrue: [^ 'For some reason, this file cannot be read' translated].
		f binary.
		((size := f size)) > 5000 & brevity
			ifTrue: [data := f next: 10000. f close. brevityState := #briefHex]
			ifFalse: [data := f contentsOfEntireFile. brevityState := #fullHex].

		s := (String new: data size*4) writeStream.
		0 to: data size-1 by: 16 do:
			[:loc | s nextPutAll: loc printStringHex; space;
				nextPut: $(; print: loc; nextPut: $); space; tab.
			loc+1 to: (loc+16 min: data size) do: [:i | s nextPutAll: (data at: i) printStringHex; space].
			s cr].
		hexData := s contents].
	
	^ contents := ((size > 5000) & brevity
		ifTrue: ['{1}
------------------------------------------
... end of the first 5000 characters.' translated format: {hexData}]
		ifFalse: [hexData]).

]

{ #category : #'file list' }
FileList >> readOnlyStream [
	"Answer a read-only stream on the selected file. For the various stream-reading services."
	^ FileStream readOnlyFileNamed: self reference fullName
]

{ #category : #accessing }
FileList >> reference [
	^ reference
]

{ #category : #accessing }
FileList >> reference: dir [
	| tmpReference |
	"Set the path of the volume to be displayed."
	self okToChange ifFalse: [^ self].
	
	tmpReference := dir 
		ifNotNil: [ :d| d asFileReference]
		ifNil: [ FileSystem disk workingDirectory ].
	tmpReference isReadable 
		ifFalse: [ ^ self inform: ('Cannot read {1}' translated format: { tmpReference})].
	reference := tmpReference.
	
	volumeList := ((Array with: '[]'), reference pathSegments)  
				withIndexCollect: [:each :i | ( String new: i-1 withAll: $ ), each].
	
	volumeListIndex  := volumeList size.	
		
	self changed: #relabel.
	self changed: #volumeList.
	self updateFileList.
]

{ #category : #'file menu action' }
FileList >> renameFile [
	"Rename the currently selected file"
	| newName response |
	listIndex = 0 ifTrue: [^ self].
	self okToChange ifFalse: [^ self].
	(response := UIManager default request: 'NewFileName?' translated
 					initialAnswer: reference basename)
		isEmptyOrNil ifTrue: [^ self].
	newName := response asFileName.
	newName = reference basename ifTrue: [^ self].
	reference renameTo: newName.
	self updateFileList.
	listIndex := list findFirst: [:item | item = newName].
	
	listIndex > 0 
		ifTrue: [reference := newName].
	self changed: #fileListIndex.

]

{ #category : #private }
FileList >> requestNewFileNameAndSave [
	| newName |
	newName := UIManager default request: 'Enter a new file name' initialAnswer: reference fullName.
	newName ifNil: [ ^ self].
	reference := newName asFileReference.
	(reference asFileReference exists) ifTrue:[ ^ self updateFileContents ].
	self put: self sourceTextModel getText asString.
	self updateFileList
]

{ #category : #private }
FileList >> selectEncoding [
	"self new selectEncoding"

	| ui |
	ui := ListDialogWindow new
		getList: [ :r | self knownEncodingIdentifiers select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		initialAnswer: '';
		acceptNewEntry: true;
		title: 'Encoding' translated;
		yourself.
	(ui chooseFromOwner: World) ifNotNil: [ :encoding | self fileEncoding: encoding ]
]

{ #category : #private }
FileList >> selectedDirectory [
	^ self directory
]

{ #category : #private }
FileList >> selectedDirectory: aFileDirectoryWrapper [
	| file |

	file := aFileDirectoryWrapper withoutListWrapper.
	file ifNil: [ 
		brevityState := #FileList.
		self changed: #fileList.
		self changed: #selectedDirectory.
		^ self ].
	self directory: file.
	brevityState := #FileList.
	"self addPath: path."
	self changed: #fileList.
	self changed: #fileListIndex.
	self changed: #contents.
	self changed: #selectedDirectory.
	self changedContents
]

{ #category : #'own services' }
FileList >> serviceAddNewDirectory [
	"Answer a service entry characterizing the 'add new directory' command"

	^ SimpleServiceEntry 
		provider: self 
		label: 'Add new directory' 
		selector: #addNewDirectory
		description: 'Adds a new, empty directory (folder)' 
]

{ #category : #'own services' }
FileList >> serviceAddNewFile [
	"Answer a service entry characterizing the 'add new file' command"

	^(SimpleServiceEntry 
		provider: self 
		label: 'Add new file' 
		selector: #addNewFile 
		description: 'Create a new,. empty file, and add it to the current directory.')
		usingLineAfter: false
]

{ #category : #'own services' }
FileList >> serviceCancel [
	"Answer a service for hitting the cancel button"

	^ (SimpleServiceEntry new
		provider: self label: 'Cancel' 
		selector: #cancelHit 
		description: 'Hit here to cancel ')
		buttonLabel: 'Cancel'
]

{ #category : #'own services' }
FileList >> serviceCopyName [

	^ (SimpleServiceEntry 
		provider: self 
		label: 'Copy name to clipboard' 
		selector: #copyName 
		description: 'Copy name to clipboard' )
]

{ #category : #'own services' }
FileList >> serviceDeleteFile [

	^ (SimpleServiceEntry 
		provider: self 
		label: 'Delete' 
		selector: #deleteFile)
		description: 'Delete the seleted item'
]

{ #category : #'own services' }
FileList >> serviceGet [
	"Answer a service for getting the entire file"

	^  (SimpleServiceEntry 
			provider: self 
			label: 'Get entire file' 
			selector: #get
			description: 'If the file has only been partially read in, because it is very large, read the entire file in at this time.')
]

{ #category : #'own services' }
FileList >> serviceGetEncodedText [

	^  (SimpleServiceEntry 
			provider: self 
			label: 'View as encoded text'
			selector: #getEncodedText
			description: 'View as encoded text')


]

{ #category : #'own services' }
FileList >> serviceGetHex [

	^  (SimpleServiceEntry 
			provider: self 
			label: 'View as hex' 
			selector: #getHex
			description: 'View as hex')
			

]

{ #category : #'own services' }
FileList >> serviceOkay [
	"Answer a service for hitting the okay button"

	^ (SimpleServiceEntry new
		provider: self 
		label: 'Okay' 
		selector: #okHit 
		description: 'Hit here to accept the current selection')
		buttonLabel: 'Ok'
]

{ #category : #'own services' }
FileList >> serviceRenameFile [

	^ (SimpleServiceEntry 
		provider: self 
		label: 'Rename' 
		selector: #renameFile 
		description: 'Rename file')
]

{ #category : #'own services' }
FileList >> serviceViewContents [
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ (SimpleServiceEntry 
		provider: self 
		label: 'Open contents' 
		selector: #viewContents)
		description: 'Open a text window whose contents are set to the contents of this file'
]

{ #category : #'own services' }
FileList >> serviceViewContentsInWorkspace [
	"Answer a service for viewing the contents of a file in a workspace"
	
	^ (SimpleServiceEntry 
		provider: self 
		label: 'Workspace with contents' 
		selector: #viewContentsInWorkspace)
		description: 'Open a new Workspace whose contents are set to the contents of this file'
]

{ #category : #'own services' }
FileList >> servicesForFolderSelector [
	"Answer the ok and cancel servies for the folder selector"

	^ self okayAndCancelServices
]

{ #category : #initialization }
FileList >> setFileStream: aStream [
	"Used to initialize a spawned file editor.  Sets directory too."
	self directory: aStream name asFileReference.
	pattern := nil.
	aStream close.
	brevityState := #needToGetBrief.
	self changed: #fileList.
	self changed: #contents.
	self changed: #pattern.
	self changed: #selectedDirectory.

]

{ #category : #private }
FileList >> setSelectedDirectoryTo: aFileDirectoryWrapper [
	self directory: aFileDirectoryWrapper withoutListWrapper.
	brevityState := #FileList.
	"self addPath: path."
	self changed: #fileList.
	self changed: #contents.
	self changed: #selectedDirectory.
]

{ #category : #'user interface' }
FileList >> setSortSelector: aByteSymbol [ 
	self sortBlock: (self perform: aByteSymbol).
]

{ #category : #private }
FileList >> sortBlock [
	"Answer block to decide what order to display the directory entries."

	^ sortBlock ifNil: [ sortBlock := self sortBlockBasename ]
]

{ #category : #private }
FileList >> sortBlock: aBlockClosure [ 
	sortBlock := aBlockClosure.
	self updateFileList
]

{ #category : #private }
FileList >> sortBlockBasename [
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x basename <= y basename  ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]
]

{ #category : #private }
FileList >> sortBlockFilesize [
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x size <= y size  ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]
]

{ #category : #private }
FileList >> sortBlockPermissions [
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x permissions <= y permissions  ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]
]

{ #category : #private }
FileList >> sortBlockTimestamp [
	"Answer block to decide what order to display the directory entries."

	^ [ :x :y | |xIsDir|
			((xIsDir := x isDirectory) = y isDirectory) 
				ifTrue: [   x modificationTime  <= y modificationTime ]
				ifFalse: [
					"directories always precede files"
					xIsDir ]]
]

{ #category : #'user interface' }
FileList >> sourceTextModel [
	^ sourceTextModel
]

{ #category : #private }
FileList >> streamFirst5000BytesFrom: aBinaryReadStream [
	^ [ self streamFirst5000BytesWithEncodingFrom: aBinaryReadStream ]
		on: Error
		do: [ :ex |(aBinaryReadStream next: 5000) asString ]
]

{ #category : #private }
FileList >> streamFirst5000BytesWithEncodingFrom: aBinaryReadStream [
	^ (ZnCharacterReadStream on: aBinaryReadStream encoding: self getEncoding) next: 5000
]

{ #category : #private }
FileList >> streamFullContentsFrom: aBinaryReadStream [
	^ [ self streamFullContentsWithEncodingFrom: aBinaryReadStream ]
		on: Error
		do: [ :ex | aBinaryReadStream contents asString ]
]

{ #category : #private }
FileList >> streamFullContentsWithEncodingFrom: aBinaryReadStream [
	^ (ZnCharacterReadStream on: aBinaryReadStream encoding: self getEncoding) upToEnd
]

{ #category : #'file list menu' }
FileList >> suffixOfSelectedFile [
	"Answer the file extension of the receiver's selected file"
	^ self class suffixOf: self fullName.
]

{ #category : #private }
FileList >> truncatedAndOverwriteFilecontents [
	self put: self sourceTextModel getText asString
]

{ #category : #initialization }
FileList >> universalButtonServices [
	"Answer the services to be reflected in the receiver's buttons"

	^ self optionalButtonSpecs
]

{ #category : #updating }
FileList >> update: aParameter [
	"Receive a change notice from an object of whom the receiver is a dependent"

	(aParameter == #fileListChanged) ifTrue: [self updateFileList].
	super update: aParameter
]

{ #category : #initialization }
FileList >> updateButtonRow [
	"Dynamically update the contents of the button row, if any."

	self dependents
		detect: [ :m | m isSystemWindow and: [ m model == self ] ]
		ifFound: [ :aWindow | 
			| aRow |
			aRow := aWindow findDeepSubmorphThat: [ :m | m hasProperty: #buttonRow ] ifAbsent: [ ^ self ].
			aRow submorphs size - 1 timesRepeat: [ aRow submorphs last delete ].
			self dynamicButtonServices
				do: [ :service | 
					aRow addMorphBack: (service buttonToTriggerIn: self).
					service addDependent: self ] ]
]

{ #category : #initialization }
FileList >> updateDirectory [
	"directory has been changed externally, by calling directory:.
	Now change the view to reflect the change."
	self changed: #directory.
	self changed: #initialDirectoryList.
	self postOpen.
]

{ #category : #private }
FileList >> updateFileContents [
	| choice |
	choice := self confirmOverwriteOrCreateNewFileOrAbort.
	
	choice = 1 ifTrue:[ self truncatedAndOverwriteFilecontents].
	choice = 2 ifTrue:[ self requestNewFileNameAndSave].

]

{ #category : #private }
FileList >> updateFileList [
	"Update my files list with file names in the current directory  
	that match the pattern.
	The pattern string may have embedded newlines or semicolons; these separate different patterns."
	list := self listForPattern: pattern.
	listIndex := self getListIndex.
	volumeListIndex := volumeList size.
	contents := ''.
	self changed: #volumeListIndex.
	self changed: #list.
	self changed: #pattern.
	self grid deselectAll.
	self grid updateList.
	self updateButtonRow
]

{ #category : #'volume list and pattern' }
FileList >> veryDeepFixupWith: deepCopier [
	super veryDeepFixupWith: deepCopier.
	volumeListIndex := 1.
	self directory: FileSystem disk workingDirectory.
	self updateFileList
]

{ #category : #'own services' }
FileList >> viewContents [
	"View the contents of my selected file in a new text window"
	
	| aString |
	
	self reference streamWritable: false do: [ :stream|
		aString := stream setConverterForCode contentsOfEntireFile ].
	
	UIManager default edit: aString label: 'Contents from ', self reference basename
]

{ #category : #'own services' }
FileList >> viewContentsInWorkspace [
	"View the contents of my selected file in a new workspace"
	
	| aString |
	
	self reference streamWritable: false do: [ :stream|
		aString := stream setConverterForCode contentsOfEntireFile ].
	
	Smalltalk tools workspace openContents: aString label: 'Workspace from ', self reference basename
]

{ #category : #'volume list and pattern' }
FileList >> volumeList [
	"Answer the current list of volumes."

	^ volumeList

]

{ #category : #'volume list and pattern' }
FileList >> volumeListIndex [
	"Answer the index of the currently selected volume."

	^ volumeListIndex

]

{ #category : #'volume list and pattern' }
FileList >> volumeListIndex: index [
	"Select the volume name having the given index."

	| delim path |
	volumeListIndex := index.
	index = 1 
		ifTrue: [self directory: FileSystem disk root ]
		ifFalse: [
			delim := reference fileSystem delimiter.
			path := String streamContents: [:stream |
				2 to: index do: [:i |
					stream nextPutAll: (volumeList at: i) trimBoth.
					i < index ifTrue: [stream nextPut: delim]]]].
	brevityState := #FileList.
	self addPath: path.
	self changed: #fileList.
	self changed: #contents.
	self updateButtonRow
]

{ #category : #'volume menu' }
FileList >> volumeMenu: aMenu [
	aMenu
		add: 'Delete directory...' translated
		selector: #deleteDirectory.
	aMenu addLine.
	aMenu
		addServices: (self itemsForDirectory: self directory)
		for: self
		extraLines: #().
	^aMenu.
]

{ #category : #'drag\'n\'drop' }
FileList >> wantsDroppedMorph: aTransferMorph event: evt inMorph: dest [
	| retval |
	retval := (aTransferMorph isTransferable)
		and: [ (aTransferMorph dragTransferType == #file)
				and: [ self isDirectoryList: dest ]].
	^retval
]

{ #category : #updating }
FileList >> whenTextAccepted: anAnnouncement [ 
	self updateFileContents 
]
