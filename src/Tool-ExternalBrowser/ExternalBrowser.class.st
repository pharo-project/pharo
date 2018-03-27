"
Browser the current image:

	ExternalBrowser openOn: CurrentImage new.

Browse a FileOut

	| internalStream |
	internalStream := (String new: 1000) writeStream.
	SystemOrganization 
		fileOutCategory: 'Tool-ExternalBrowser'
		on: internalStream.
	ExternalBrowser browseStream: internalStream contents readStream.
"
Class {
	#name : #ExternalBrowser,
	#superclass : #ComposablePresenter,
	#instVars : [
		'classes',
		'packages',
		'protocols',
		'selectors',
		'method',
		'showClassSide',
		'model'
	],
	#category : #'Tool-ExternalBrowser'
}

{ #category : #'public access' }
ExternalBrowser class >> browseStream: aStream [

	self openOn: (RingChunkImporter fromStream: aStream)
]

{ #category : #specs }
ExternalBrowser class >> defaultSpec [
	<spec: #default>
	
	^ SpecLayout composed 
		newColumn: [ :col | 
			col newRow: [ :row | 
					row newColumn: [ :subcolumn |
						subcolumn add: #packages.
						subcolumn add: #showClassSide height: 16  ].
					row 
						add: #classes;
						add: #protocols;
						add: #selectors ] ;
			newRow: [ :row | 
					row add: #method ]]; 
		yourself
]

{ #category : #'System-FileRegistry' }
ExternalBrowser class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	^ (#('st' 'cs') includes: suffix)
		ifTrue: [ Array with: self serviceBrowseCode]
		ifFalse: [ #() ].
]

{ #category : #specs }
ExternalBrowser class >> openOn: model [

	self basicNew
		model: model;
		initialize;
		openWithSpec
]

{ #category : #'System-FileRegistry' }
ExternalBrowser class >> serviceBrowseCode [
	"Answer the service of opening a file-contents browser"

	^ (SimpleServiceEntry
		provider: self 
		label: 'Code-file browser'
		selector: #browseStream:
		description: 'Open a "file-contents browser" on this file, allowing you to view and selectively load its code'
		buttonLabel: 'Code')
		argumentGetter: [ :file| file readOnlyStream ]
]

{ #category : #'System-FileRegistry' }
ExternalBrowser class >> services [
	"Answer potential file services associated with this class"

	^ {self serviceBrowseCode}.
]

{ #category : #accessing }
ExternalBrowser >> classes [

	^ classes
]

{ #category : #'structure accessing' }
ExternalBrowser >> classesOfPackage: aPackage [
	| theClasses |
	theClasses := aPackage classes.
	^ self showClassSide state
		ifTrue: [ theClasses collect: #classSide ]
		ifFalse: [ theClasses ]
]

{ #category : #initialize }
ExternalBrowser >> initialExtent [

	^ 800@600
]

{ #category : #initialize }
ExternalBrowser >> initializeClassList [
	classes := self newList
]

{ #category : #initialize }
ExternalBrowser >> initializeMethodBodyPane [
	method := self newCode
]

{ #category : #initialize }
ExternalBrowser >> initializePackageList [
	packages := self newList.
	self populatePackages.
	packages displayBlock: [ :m | m name ]
]

{ #category : #initialize }
ExternalBrowser >> initializePresenter [
	self wirePackages.
	self wireClasses.
	self wireProtocols.
	self wireSelectors.
	
	showClassSide 
		whenChangedDo: [ self refreshClasses: self packages selectedItem ].
]

{ #category : #initialize }
ExternalBrowser >> initializeProtocolList [
	protocols := self newList
]

{ #category : #initialize }
ExternalBrowser >> initializeSelectorList [

	selectors := self newList.
	
]

{ #category : #initialize }
ExternalBrowser >> initializeWidgets [
	self initializePackageList.
	self initializeClassList.
	self initializeProtocolList.
	self initializeSelectorList.
	self initializeMethodBodyPane.
	
	showClassSide := self newCheckBox.
	showClassSide label: 'Class side'.
	showClassSide state: false.
	
	self focusOrder
		add: packages;
		add: classes;
		add: protocols;
		add: selectors;
		add: method
]

{ #category : #accessing }
ExternalBrowser >> method [
	^ method
]

{ #category : #accessing }
ExternalBrowser >> method: anObject [
	method := anObject
]

{ #category : #'structure accessing' }
ExternalBrowser >> methodFor: aSelector [

	^ (self classes selectedItem methodNamed: aSelector) sourceCode
			
]

{ #category : #accessing }
ExternalBrowser >> model: aModel [

	model := aModel
]

{ #category : #accessing }
ExternalBrowser >> packages [

	^ packages
]

{ #category : #'selection operation' }
ExternalBrowser >> populatePackages [

	packages items: model packages.
]

{ #category : #accessing }
ExternalBrowser >> protocols [

	^ protocols
]

{ #category : #'structure accessing' }
ExternalBrowser >> protocolsOfClass: aClass [

	^ aClass protocols
]

{ #category : #initialize }
ExternalBrowser >> refreshClasses: pack [
	self resetClassSelection.
	pack 
		ifNil: [ classes items: #() ] 
		ifNotNil: [ classes items: (self classesOfPackage: pack)]. 
	Transcript show: pack printString;cr
]

{ #category : #'selection operation' }
ExternalBrowser >> resetClassSelection [

	self classes resetSelection
]

{ #category : #'selection operation' }
ExternalBrowser >> resetMethodBodyPane [
	self method text: ''
	
]

{ #category : #'selection operation' }
ExternalBrowser >> resetProtocolSelection [

	self protocols resetSelection
]

{ #category : #'selection operation' }
ExternalBrowser >> resetSelectorSelection [

	self selectors resetSelection
]

{ #category : #accessing }
ExternalBrowser >> selectors [
	^ selectors
]

{ #category : #accessing }
ExternalBrowser >> selectors: anObject [
	selectors := anObject
]

{ #category : #'structure accessing' }
ExternalBrowser >> selectorsOfProtocol: aProtocol [ 
	
	| theSelectedClass |
	theSelectedClass := self showClassSide state
		ifFalse: [ self classes selectedItem ]
		ifTrue: [ self classes selectedItem classSide ].
	^ (theSelectedClass 
		methodsInProtocol: self protocols selectedItem) collect: #selector
]

{ #category : #'structure accessing' }
ExternalBrowser >> showClassDefinition [
	method text: (self classes selectedItem definition  ifNil:[''])
]

{ #category : #accessing }
ExternalBrowser >> showClassSide [

	^ showClassSide
]

{ #category : #accessing }
ExternalBrowser >> title [ 
	^ 'External Package Browser'
]

{ #category : #initialize }
ExternalBrowser >> wireClasses [
	classes
		whenSelectedItemChanged: [ :class | 
			self resetProtocolSelection.
			class 
				ifNil: [ protocols items: #() ] 
				ifNotNil: [ protocols items: (self protocolsOfClass: class).
					         self showClassDefinition ] ]
]

{ #category : #initialize }
ExternalBrowser >> wirePackages [
	packages whenSelectedItemChanged: [ :pack | 
		self refreshClasses: pack ].


	
]

{ #category : #initialize }
ExternalBrowser >> wireProtocols [
	protocols
		whenSelectedItemChanged: [ :protocol | 
			self resetSelectorSelection.
			protocol 
				ifNil: [ selectors items: #() ] 
				ifNotNil: [ selectors items: (self selectorsOfProtocol: protocol) ] ]
]

{ #category : #initialize }
ExternalBrowser >> wireSelectors [
	selectors
		whenSelectedItemChanged: [ :selector | 
			selector 
				ifNil: [ self resetMethodBodyPane ] 
				ifNotNil: [ method text:  (self methodFor: selector) ]]
]
