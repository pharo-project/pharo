"
Browser for snapshots
"
Class {
	#name : 'MCSnapshotBrowser',
	#superclass : 'Model',
	#instVars : [
		'categorySelection',
		'classSelection',
		'protocolSelection',
		'methodSelection',
		'switch',
		'items',
		'modal',
		'morph',
		'label'
	],
	#category : 'MonticelloGUI-Tools',
	#package : 'MonticelloGUI',
	#tag : 'Tools'
}

{ #category : 'instance creation' }
MCSnapshotBrowser class >> forSnapshot: aSnapshot [

	^ self new snapshot: aSnapshot
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> accept [
	" do nothing by default"
]

{ #category : 'accessing' }
MCSnapshotBrowser >> allClassNames [
	^ (items 
		select: [:ea | (ea isOrganizationDefinition | ea isScriptDefinition) not]
		thenCollect: [:ea | ea className]) asSet.

]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> arrowKey: aCharacter from: aPluggableListMorph [ 
	"backstop"
]

{ #category : 'menus' }
MCSnapshotBrowser >> browseMessages [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all implementors of the selector chosen."

	self systemNavigation browseAllImplementorsOf: (self selectedMessageName ifNil: [ ^nil ])
]

{ #category : 'menus' }
MCSnapshotBrowser >> browseMethodFull [
	"Create and schedule a full Browser and then select the current class and message."

	| myClass |
	(myClass := self selectedClassOrMetaClass) ifNotNil:
		[Smalltalk tools browser openOnClass: myClass selector: self selectedMessageName]
]

{ #category : 'menus' }
MCSnapshotBrowser >> browseSendersOfMessages [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self systemNavigation browseAllSendersOf: (self selectedMessageName ifNil: [ ^nil ])
]

{ #category : 'menus' }
MCSnapshotBrowser >> browseVersions [
	"Create and schedule a message set browser on all versions of the 
	currently selected message selector."
	
	Smalltalk tools versionBrowser  
		browseVersionsForClass:  self selectedClassOrMetaClass 
		selector: self selectedMessageName
		
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buildWindow [
	| window |
	window := SystemWindow labelled: self label.
	window model: self.
	self widgetSpecs do: [:spec |
		| send fractions offsets |
		send := spec first.
		fractions := spec at: 2 ifAbsent: [#(0 0 1 1)].
		offsets := spec at: 3 ifAbsent: [#(0 0 0 0)].
		window
			addMorph: (self perform: send first withArguments: send allButFirst)
			fullFrame:
				(LayoutFrame new 
					leftFraction: fractions first; 
					topFraction: fractions second;
					rightFraction: fractions third ; 
					bottomFraction: fractions fourth; 
					leftOffset: offsets first;
					topOffset: offsets second;
					rightOffset: offsets third;
					bottomOffset: offsets fourth)].
	^ window
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buttonEnabled [
	^ true
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buttonRow [
	^ self buttonRow: self buttonSpecs
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buttonRow: specArray [
	| aRow |
	aRow := PanelMorph new.
	aRow layoutPolicy: TableLayout new; listDirection: #leftToRight.
	aRow hResizing: #spaceFill; vResizing: #spaceFill; rubberBandCells: true.
	aRow clipSubmorphs: true; borderWidth: 0.
	aRow layoutInset: 2@2; cellInset: 1.
	aRow wrapCentering: #center; cellPositioning: #leftCenter.
	specArray do:
		[:triplet | | aButton state |
			state := triplet at: 5 ifAbsent: [#buttonState].
			aButton := PluggableButtonMorph
				on: self
				getState: state
				action: #performButtonAction:enabled:.
			aButton
				hResizing: #spaceFill;
				vResizing: #spaceFill;
				label: triplet first asString;
				getEnabledSelector: (triplet at: 4 ifAbsent: [#buttonEnabled]);
				arguments: (Array with: triplet second with: (triplet at: 4 ifAbsent: [#buttonEnabled])).
			aRow addMorphBack: aButton.
			aButton setBalloonText: triplet third].
	^ aRow
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buttonSelected [
	^ false
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buttonSpecs [
	^ #(('instance' switchBeInstance 'show instance' buttonEnabled switchIsInstance)
		('?' switchBeComment 'show comment' buttonEnabled switchIsComment)
		('class' switchBeClass 'show class' buttonEnabled switchIsClass))
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> buttonState [
	^ false
]

{ #category : 'listing' }
MCSnapshotBrowser >> categoryList [
	^ self visibleCategories
]

{ #category : 'menus' }
MCSnapshotBrowser >> categoryListMenu: aMenu [ 
	categorySelection
		ifNotNil: [aMenu
				add: (categorySelection = '*Extensions'
						ifTrue: ['Load all extension methods']
						ifFalse: ['Load class category {1}' format: {categorySelection}])
				selector: #loadCategorySelection].
	^ aMenu
]

{ #category : 'selecting' }
MCSnapshotBrowser >> categorySelection [
	^ categorySelection ifNil: [0] ifNotNil: [self visibleCategories indexOf: categorySelection]
]

{ #category : 'selecting' }
MCSnapshotBrowser >> categorySelection: aNumber [
	categorySelection := aNumber = 0 ifFalse: [self visibleCategories at: aNumber].
	self classSelection: 0.
	self changed: #categorySelection;
		changed: #classList.

]

{ #category : 'text' }
MCSnapshotBrowser >> classCommentString [
	^ items
		detect: [ :ea | ea isClassDefinition and: [ ea className = classSelection ] ]
		ifFound: [ :classDefinition | classDefinition comment ]
		ifNone: [ '' ]
]

{ #category : 'text' }
MCSnapshotBrowser >> classDefinitionString [
	| defs |
	defs := items select: [:ea | (ea isClassDefinition or: [ea isClassDefinitionExtension])
			and: [ea className = classSelection]].

	defs isEmpty ifTrue: [^ 'This class is defined elsewhere.'].

	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | ea printDefinitionOn: stream]
			separatedBy: [stream nextPut: $.; cr]
	].
]

{ #category : 'menus' }
MCSnapshotBrowser >> classHierarchy [
	"Create and schedule a class list browser on the receiver's hierarchy."

	self systemNavigation 
		browseHierarchy: self selectedClassOrMetaClass
		selector: self selectedMessageName	"OK if nil"
]

{ #category : 'listing' }
MCSnapshotBrowser >> classList [
	^ self visibleClasses
]

{ #category : 'menus' }
MCSnapshotBrowser >> classListMenu: aMenu [

	classSelection ifNil: [ ^ aMenu ].
	aMenu
		addList: #( #- #( 'Browse full (b)' browseMethodFull ) #( 'Browse hierarchy (h)' classHierarchy ) #- #( 'Show hierarchy' methodHierarchy ) );
		addLine;
		add: ('Load class {1}' format: { classSelection }) selector: #loadClassSelection.
	^ aMenu
]

{ #category : 'selecting' }
MCSnapshotBrowser >> classSelection [
	^ classSelection ifNil: [0] ifNotNil: [self visibleClasses indexOf: classSelection]
]

{ #category : 'selecting' }
MCSnapshotBrowser >> classSelection: aNumber [
	classSelection := aNumber = 0 ifFalse: [self visibleClasses at: aNumber].
	self protocolSelection: 0.
	self changed: #classSelection; 
		changed: #protocolList;
		changed: #methodList.

]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> close [
	self window delete
]

{ #category : 'menus' }
MCSnapshotBrowser >> copySelector [
	"Copy the selected selector to the clipboard"

	| selector |
	(selector := self selectedMessageName) ifNotNil:
		[Clipboard clipboardText: selector asString]
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> defaultLabel [
	^ 'Snapshot Browser'
]

{ #category : 'accessing' }
MCSnapshotBrowser >> extensionClassNames [
	^ (self allClassNames difference: self packageClassNames) asSortedCollection
]

{ #category : 'accessing' }
MCSnapshotBrowser >> extensionsCategory [
	^ '*Extensions'
]

{ #category : 'menus' }
MCSnapshotBrowser >> fileOutMessage [
	"Put a description of the selected message on a file"

	| fileName |
	self selectedMessageName ifNotNil: [
		Cursor write showWhile: [
			self selectedClassOrMetaClass fileOutMethod:
				self selectedMessageName ].
		^ self ].
	items isEmpty ifTrue: [ ^ self ].
	fileName := MorphicUIManager new
		            request: 'File out on which file?'
		            initialAnswer: 'methods'.
	Cursor write showWhile: [
		| internalStream |
		internalStream := WriteStream on: (String new: 1000).
		internalStream
			header;
			timeStamp.
		items do: [ :patchOp |
			patchOp definition isMethodDefinition ifTrue: [
				(patchOp definition actualClass isNotNil and: [
					 patchOp definition actualClass includesSelector:
						 patchOp definition selector ])
					ifTrue: [
						patchOp definition actualClass
							printMethodChunk: patchOp definition selector
							on: internalStream ]
					ifFalse: [
						internalStream nextChunkPut:
							patchOp definition className , ' removeSelector: '
							, patchOp definition selector printString ] ].
			patchOp definition isClassDefinition ifTrue: [
				patchOp definition actualClass
					ifNotNil: [
						internalStream nextChunkPut:
							patchOp definition actualClass definition.
						patchOp definition comment ifNotNil: [
							patchOp definition actualClass putCommentOnFile: internalStream ] ]
					ifNil: [
						internalStream nextChunkPut:
							patchOp definition className , ' removeFromSystem' ] ] ].
		CodeExporter
			writeSourceCodeFrom: internalStream
			baseName: fileName
			isSt: true ]
]

{ #category : 'accessing' }
MCSnapshotBrowser >> hasExtensions [
	^self extensionClassNames notEmpty
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> initialExtent [
	^ 650@400.
]

{ #category : 'menus' }
MCSnapshotBrowser >> inspectSelection [
	^ self methodSelection inspect
]

{ #category : 'utilities' }
MCSnapshotBrowser >> interactionModel [
	^ self
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> label [
	^ label ifNil: [self defaultLabel]
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> label: aString [

	label := aString
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> listMorph: listSymbol [

	| selectionSymbol |
	selectionSymbol := (listSymbol , 'Selection') asSymbol.

	^ PluggableListMorph
		  on: self
		  list: (listSymbol , 'List') asSymbol
		  selected: selectionSymbol
		  changeSelected: (selectionSymbol , ':') asSymbol
		  menu: (listSymbol , 'ListMenu:') asSymbol
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> listMorph: listSymbol selection: selectionSymbol menu: menuSymbol keystroke: keystrokeSymbol [
	^ (PluggableListMorph
		on: self
		list: listSymbol
		selected: selectionSymbol
		changeSelected: (selectionSymbol, ':') asSymbol
		menu: menuSymbol)
		keystrokeActionSelector: keystrokeSymbol;
		yourself
]

{ #category : 'menus' }
MCSnapshotBrowser >> loadCategorySelection [
	"Load the entire selected category"
	categorySelection ifNil: [ ^self ].
	self methodsForSelectedClassCategory do: [ :m | m load ].
]

{ #category : 'menus' }
MCSnapshotBrowser >> loadClassSelection [
	classSelection ifNil: [ ^ self ].
	self packageClasses
		detect: [ :ea | ea className = classSelection ]
		ifFound: [ :packageClass | 
			packageClass load.
			self methodsForSelectedClass do: [ :m | m load ] ]
]

{ #category : 'menus' }
MCSnapshotBrowser >> loadMethodSelection [
	methodSelection ifNil: [ ^self ].
	methodSelection load.
]

{ #category : 'menus' }
MCSnapshotBrowser >> loadProtocolSelection [
	protocolSelection ifNil: [ ^self ].
	self methodsForSelectedProtocol do: [ :m | m load ].
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> menu [
	" returns nil to let the editing mode offer the right menu"
	^ nil
]

{ #category : 'text' }
MCSnapshotBrowser >> metaclassDefinitionString [
	| defs |
	defs := items select: [:ea | (ea isClassDefinition or: [ea isClassDefinitionExtension])
			and: [ea className = classSelection]].

	defs isEmpty ifTrue: [^ 'This class is defined elsewhere.'].

	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | ea printClassDefinitionOn: stream]
			separatedBy: [stream nextPut: $.; cr]
	].
]

{ #category : 'menus' }
MCSnapshotBrowser >> methodHierarchy [
	"Create and schedule a method browser on the hierarchy of implementors."

	self systemNavigation methodHierarchyBrowserForClass: self selectedClassOrMetaClass
		selector: self selectedMessageName
]

{ #category : 'listing' }
MCSnapshotBrowser >> methodList [
	^ self visibleMethods collect: [:ea | ea selector]
]

{ #category : 'menus' }
MCSnapshotBrowser >> methodListKey: aKeystroke from: aListMorph [ 
	aKeystroke caseOf: {
		[$b] -> [self browseMethodFull].
		[$h] -> [self classHierarchy].
		[$o] -> [self fileOutMessage].
		[$c] -> [self copySelector].
		[$n] -> [self browseSendersOfMessages].
		[$m] -> [self browseMessages].
		[$i] -> [self methodHierarchy].
		[$v] -> [self browseVersions]}
		 otherwise: []
]

{ #category : 'menus' }
MCSnapshotBrowser >> methodListMenu: aMenu [

	self selectedMessageName
		ifNil: [ items isNotEmpty ifTrue: [ aMenu add: 'FileOut (o)' selector: #fileOutMessage ] ]
		ifNotNil: [
			aMenu addList:
				#( #( 'Browse full (b)' browseMethodFull ) #( 'Browse hierarchy (h)' classHierarchy ) #- #( 'FileOut (o)' fileOutMessage ) #( 'Copy selector (c)'
				      copySelector ) ).
			aMenu addList: #( #- #( 'Browse senders (n)' browseSendersOfMessages ) #( 'Browse implementors (m)' browseMessages )
				   #( 'Inheritance (i)' methodHierarchy ) #( 'Versions (v)' browseVersions ) ) ].


	(self selectedMessageName isNotNil and: [ methodSelection isLoadable ]) ifTrue: [
		aMenu
			addLine;
			add: 'Load method' selector: #loadMethodSelection ].
	^ aMenu
]

{ #category : 'selecting' }
MCSnapshotBrowser >> methodSelection [
	^ methodSelection
			ifNil: [0] 
			ifNotNil: [self visibleMethods indexOf: methodSelection]
]

{ #category : 'selecting' }
MCSnapshotBrowser >> methodSelection: aNumber [
	methodSelection := aNumber = 0 ifFalse: [self visibleMethods at: aNumber].
	self changed: #methodSelection; changed: #text.
]

{ #category : 'accessing' }
MCSnapshotBrowser >> methodsForSelectedClass [

	^ items select: [ :ea | ea className = classSelection and: [ ea isMethodDefinition and: [ ea classIsMeta = self switchIsClass ] ] ]
]

{ #category : 'accessing' }
MCSnapshotBrowser >> methodsForSelectedClassCategory [
	| visibleClasses |
	visibleClasses := self visibleClasses.
	^ items select: [:ea | (visibleClasses includes: ea className) 
								and: [ea isMethodDefinition
									and: [ea classIsMeta = self switchIsClass]]].
]

{ #category : 'accessing' }
MCSnapshotBrowser >> methodsForSelectedProtocol [
	| methods |
	protocolSelection ifNil: [^ Array new].
	methods := self methodsForSelectedClass asOrderedCollection.
	(protocolSelection = '-- all --') 
		ifFalse: [methods removeAllSuchThat: [:ea | ea protocol ~= protocolSelection]].
	^ methods 
	
								
]

{ #category : 'accessing' }
MCSnapshotBrowser >> packageClassNames [
	^ self packageClasses collect: [:ea | ea className]
]

{ #category : 'accessing' }
MCSnapshotBrowser >> packageClasses [
	^ items select: [:ea | ea isClassDefinition]
]

{ #category : 'listing' }
MCSnapshotBrowser >> packageOrganizations [
	^ items select: [:ea | ea isOrganizationDefinition]
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> performButtonAction: anActionSelector enabled: anEnabledSelector [
	(self perform: anEnabledSelector) 
		ifTrue: [ self perform: anActionSelector ]
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> preferredColor [  
	^ (Color r: 0.627 g: 0.69 b: 0.976)
]

{ #category : 'listing' }
MCSnapshotBrowser >> protocolList [
	^ self visibleProtocols
]

{ #category : 'menus' }
MCSnapshotBrowser >> protocolListMenu: aMenu [ 
	protocolSelection
		ifNotNil: [aMenu
				add: ('Load protocol ''{1}''' format: {protocolSelection})
				selector: #loadProtocolSelection ].
	^ aMenu
]

{ #category : 'selecting' }
MCSnapshotBrowser >> protocolSelection [
	^ protocolSelection 
		ifNil: [0]
		ifNotNil: [self visibleProtocols indexOf: protocolSelection]
]

{ #category : 'selecting' }
MCSnapshotBrowser >> protocolSelection: anInteger [
	protocolSelection := (anInteger = 0 ifFalse: [self visibleProtocols at: anInteger]).
	self methodSelection: 0.
	self changed: #protocolSelection;
		changed: #methodList.
]

{ #category : 'text' }
MCSnapshotBrowser >> scriptDefinitionString [
	| defs |
	defs := items select: [:ea | ea isScriptDefinition].
	defs isEmpty ifTrue: [^'(package defines no scripts)'].
	
	^ String streamContents: [:stream | 
		defs asArray sort 
			do: [:ea | stream nextPutAll: '---------- package ';
					nextPutAll: ea scriptSelector;
					nextPutAll: ' ----------'; cr;
					nextPutAll: ea script; cr]
			separatedBy: [stream cr]].
]

{ #category : 'accessing' }
MCSnapshotBrowser >> selectedClass [
	classSelection ifNil: [ ^ nil ].
	^ Smalltalk globals at: classSelection ifAbsent: [ nil ]
]

{ #category : 'accessing' }
MCSnapshotBrowser >> selectedClassOrMetaClass [
	| class |
	classSelection ifNil: [ ^ nil ].
	class := Smalltalk globals at: classSelection ifAbsent: [ ^ nil ].
	^ self switchIsClass
		ifTrue: [ class class ]
		ifFalse: [ class ]
]

{ #category : 'accessing' }
MCSnapshotBrowser >> selectedMessageCategoryName [
	^protocolSelection
]

{ #category : 'accessing' }
MCSnapshotBrowser >> selectedMessageName [
	^methodSelection ifNotNil: [^ methodSelection selector ].

]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> shoutAboutToStyle: aPluggableShoutMorphOrView [
	^ false
]

{ #category : 'opening' }
MCSnapshotBrowser >> show [
	"Open the tool returning the window."

	modal := false.
	self window openInWorld.
	^ self window
]

{ #category : 'opening' }
MCSnapshotBrowser >> showLabelled: labelString [

	modal := false.
	self label: labelString.
	self window openInWorld.
	^ self window
]

{ #category : 'switch' }
MCSnapshotBrowser >> signalSwitchChanged [
	self protocolSelection: 0.
	self 
		changed: #switchIsInstance;
		changed: #switchIsComment;
		changed: #switchIsClass;
		changed: #protocolList;
		changed: #methodList;
		changed: #text.
]

{ #category : 'accessing' }
MCSnapshotBrowser >> snapshot: aSnapshot [
	items := aSnapshot definitions asSortedCollection.
	self categorySelection: 0.
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> summary: aString [
	" do nothing by default"
]

{ #category : 'switch' }
MCSnapshotBrowser >> switchBeClass [
	switch := #class.
	self signalSwitchChanged.
]

{ #category : 'switch' }
MCSnapshotBrowser >> switchBeComment [
	switch := #comment.
	self signalSwitchChanged.
]

{ #category : 'switch' }
MCSnapshotBrowser >> switchBeInstance [
	switch := #instance.
	self signalSwitchChanged.
]

{ #category : 'switch' }
MCSnapshotBrowser >> switchIsClass [
	^ switch = #class
]

{ #category : 'switch' }
MCSnapshotBrowser >> switchIsComment [
	^ switch = #comment.
]

{ #category : 'switch' }
MCSnapshotBrowser >> switchIsInstance [
	switch ifNil: [switch := #instance].
	^ switch = #instance.
]

{ #category : 'text' }
MCSnapshotBrowser >> text [
	self switchIsComment ifTrue: [ ^ self classCommentString ].
	methodSelection ifNotNil: [ ^ methodSelection source ].
	protocolSelection ifNotNil: [ ^ '' ].
	classSelection ifNotNil: [
		^ self switchIsClass 
			ifTrue: [ self metaclassDefinitionString ]
			ifFalse: [ self classDefinitionString ] ].
	categorySelection ifNil: [ ^ self scriptDefinitionString ].
	^ ''
]

{ #category : 'text' }
MCSnapshotBrowser >> text: aTextOrString [
	self changed: #text
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> textMorph: aSymbol [
	| textMorph |
	textMorph := RubPluggableTextMorph new
		getTextSelector: aSymbol;
		setTextSelector: (aSymbol , ':') asSymbol;
		on: self;
		beWrapped;
		hScrollbarShowNever;
		beForSmalltalkScripting;
		yourself.
	textMorph announcer when: RubTextAcceptRequest send: #accept to: self.
	textMorph hasUnacceptedEdits: false.
	^ textMorph
]

{ #category : 'listing' }
MCSnapshotBrowser >> visibleCategories [
	^ ((self packageOrganizations flatCollect: [ :ea | ea categories ]),
		(self packageClasses collect: [ :ea | ea category ]),
			(self hasExtensions ifTrue: [{ self extensionsCategory }] ifFalse: [#()]))
				asSet asSortedCollection
]

{ #category : 'listing' }
MCSnapshotBrowser >> visibleClasses [
	^ categorySelection = self extensionsCategory
		ifTrue: [self extensionClassNames]
		ifFalse: [self packageClasses
					select: [:ea | ea category = categorySelection]
					thenCollect: [:ea | ea className]].
]

{ #category : 'listing' }
MCSnapshotBrowser >> visibleMethods [
	^ classSelection 
		ifNil: [#()]
		ifNotNil: [self methodsForSelectedProtocol]
]

{ #category : 'listing' }
MCSnapshotBrowser >> visibleProtocols [

	| methods |
	self switchIsComment ifTrue: [ ^ Array new ].
	methods := self methodsForSelectedClass.
	^ (methods collect: [ :ea | ea category ]) asSet asSortedCollection
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> widgetSpecs [

	 ^#(
		((listMorph: category) (0 0 0.25 0.4))
		((listMorph: class) (0.25 0 0.50 0.4) (0 0 0 -30))
		((listMorph: protocol) (0.50 0 0.75 0.4))
		((listMorph:selection:menu:keystroke:  methodList methodSelection methodListMenu: methodListKey:from:) (0.75 0 1 0.4))
		((buttonRow) (0.25 0.4 0.5 0.4) (0 -30 0 0))
		((textMorph: text) (0 0.4 1 1))
		) 
]

{ #category : 'morphic ui' }
MCSnapshotBrowser >> window [

	^ morph ifNil: [ morph := self buildWindow ]
]
