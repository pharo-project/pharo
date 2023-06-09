"
Browser the current image:

	ExternalBrowser openOn: Smalltalk image.

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
	#superclass : #SpPresenterWithModel,
	#instVars : [
		'classes',
		'packages',
		'protocols',
		'selectors',
		'method',
		'showClassSide'
	],
	#category : #'Tool-ExternalBrowser-Browsers'
}

{ #category : #'public access' }
ExternalBrowser class >> browseStream: aStream [

	self openOn: (RingChunkImporter fromStream: aStream)
]

{ #category : #layout }
ExternalBrowser class >> defaultLayout [
	<spec: #default>

	^ SpPanedLayout newTopToBottom
		add: (SpBoxLayout newTopToBottom
			add: (
				SpPanedLayout newLeftToRight
					add:(
						SpPanedLayout newLeftToRight
							add: #packages;
							add: #classes;
							yourself);
					add: (
						SpPanedLayout newLeftToRight
							add: #protocols;
							add: #selectors;
							yourself);
					yourself
			);
			add: #showClassSide height: self labelHeight;
			yourself
		);
		add: #method;
		yourself
]

{ #category : #examples }
ExternalBrowser class >> example [
	<example>

	self openOn: Smalltalk image
]

{ #category : #'System-FileRegistry' }
ExternalBrowser class >> fileReaderServicesForFile: fullName suffix: suffix [
	<fileService>

	^ (self sourceFileSuffixes includes: suffix)
		ifTrue: [ Array with: self serviceBrowseCode]
		ifFalse: [ #() ]
]

{ #category : #'instance creation' }
ExternalBrowser class >> openOn: model [

	self basicNew
		model: model;
		initialize;
		open
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
		argumentGetter: [ :file| file readStream]
]

{ #category : #'System-FileRegistry' }
ExternalBrowser class >> services [
	"Answer potential file services associated with this class"

	^ {self serviceBrowseCode}
]

{ #category : #'System-FileRegistry' }
ExternalBrowser class >> sourceFileSuffixes [

	^#('st' 'cs')
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
		ifTrue: [ theClasses collect: [:each | each classSide] ]
		ifFalse: [ theClasses ]
]

{ #category : #initialization }
ExternalBrowser >> connectPresenters [

	packages display: [ :m | m name ].

	packages whenSelectionChangedDo: [ :selection |
		self refreshClasses: selection selectedItem ].

	classes whenSelectionChangedDo: [ :selection |
		self refreshProtocols: selection ].

	protocols whenSelectionChangedDo: [ :selection |
		self selectors resetListSelection.
		selection selectedItem
			ifNil: [ selectors items: #(  ) ]
			ifNotNil: [
			selectors items: (self selectorsOfProtocol: selection selectedItem) ] ].

	selectors whenSelectionChangedDo: [ :selection |
		selection selectedItem
			ifNil: [
				self method text: '' ]
			ifNotNil: [ method text: (self methodFor: selection selectedItem) ] ].

	showClassSide whenChangedDo: [
		self refreshClasses: self packages selectedItem ]
]

{ #category : #'initialization - deprecated' }
ExternalBrowser >> initializePresenter [

	packages := self newList.
	classes := self newList.
	protocols := self newList.
	selectors := self newList.
	method := self newCode withoutSyntaxHighlight.

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

{ #category : #initialization }
ExternalBrowser >> initializeWindow: aWindow [

	aWindow title: 'External Package Browser'.
	aWindow initialExtent: 800@600
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
ExternalBrowser >> packages [

	^ packages
]

{ #category : #accessing }
ExternalBrowser >> protocols [

	^ protocols
]

{ #category : #'structure accessing' }
ExternalBrowser >> protocolsOfClass: aClass [

	^ aClass protocolNames
]

{ #category : #refresh }
ExternalBrowser >> refreshClasses: pack [
	self classes resetListSelection .
	pack
		ifNil: [ classes items: #() ]
		ifNotNil: [ classes items: (self classesOfPackage: pack) asOrderedCollection].
]

{ #category : #initialize }
ExternalBrowser >> refreshProtocols: selection [

	self protocols resetListSelection.
	^ selection selectedItem
		  ifNil: [ protocols items: #(  ) ]
		  ifNotNil: [
			  protocols items: (self protocolsOfClass: selection selectedItem) asSortedCollection.
			  self showClassDefinition ]
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
		methodsInProtocol: self protocols selectedItem) asArray collect: [:each | each selector]
]

{ #category : #'structure accessing' }
ExternalBrowser >> showClassDefinition [

	method text: (self classes selectedItem definitionString ifNil: [ '' ])
]

{ #category : #accessing }
ExternalBrowser >> showClassSide [

	^ showClassSide
]

{ #category : #updating }
ExternalBrowser >> updatePresenter [

	packages items: self model packages
]
