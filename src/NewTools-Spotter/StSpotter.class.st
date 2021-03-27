"
When exploring the system, is useful to have a single entry point that will allow users to search for different component of the system. 
Spotter provides such entry point and is usually available by pressing <meta+enter>.

Spotter is just a front-end to show the result of different processors (see *StSpotterProcessor*) that provide results. Those processors can be configured in different ways and will provide different access options. 

Some known processors and options are: 

## Classes processor: 
- Type #classes in the search bar
- Press <meta+b>

## Implementors processor: 
- Type #implementors in the search bar
- Press <meta+m>

## Senders processor: 
- Type #senders in the search bar
- Press <meta+n>
"
Class {
	#name : #StSpotter,
	#superclass : #StPresenter,
	#instVars : [
		'searchText',
		'resultList',
		'model',
		'previewContainer',
		'categories',
		'mutex',
		'spinner',
		'tip'
	],
	#classVars : [
		'History',
		'Preview',
		'Tips'
	],
	#classInstVars : [
		'spotterWindow'
	],
	#category : #'NewTools-Spotter-View'
}

{ #category : #private }
StSpotter class >> basicHistory [

	^ History ifNil: [ History := WeakOrderedCollection new ]
]

{ #category : #private }
StSpotter class >> createTips [

	^ {
		'<meta+n> filters for Classes'.
		'<meta+m> filters for Implementros'.
		'<meta+h> filters for Help topics'.
		'Adding #Classes in the query filters for Classes'.
		'Adding #Implementors in the query filters for Implementros'.
		'Adding #Help in the query filters for Help Topics'.
		'Adding #Packages in the query filters for Packages'.
		'Adding #Menu in the query filters for System Menu entries'.
		'Using words separated with spaces looks up for all of them'.
		'Using camel case looks up for all of the parts'.
		'Clicking in the question mark shows another tip'.
		'Clicking in this tip shows another tip'.
	}
]

{ #category : #accessing }
StSpotter class >> hidePreview [
	<script>

	Preview := false
]

{ #category : #private }
StSpotter class >> historize: aCandidateLink [

	self basicHistory 
		removeAllSuchThat: [ :each | each value = aCandidateLink value ];
		addFirst: aCandidateLink
]

{ #category : #accessing }
StSpotter class >> history [

	^ self basicHistory reject: [ :each | each isNil ]
]

{ #category : #'class initialization' }
StSpotter class >> initialize [

	self showPreview
]

{ #category : #testing }
StSpotter class >> isShowingPreview [

	^ Preview
]

{ #category : #'instance creation' }
StSpotter class >> open [

	self reset.
	^ spotterWindow := self new openModalWithSpec
]

{ #category : #settings }
StSpotter class >> previewVisible [
		
	^ Preview
]

{ #category : #settings }
StSpotter class >> previewVisible: aBoolean [
	
	Preview := aBoolean
]

{ #category : #'tools registry' }
StSpotter class >> registerToolsOn: registry [
	"self registerToolsOn: Smalltalk tools"

	registry register: self as: #spotter
]

{ #category : #private }
StSpotter class >> reset [
	
	spotterWindow ifNotNil: [ spotterWindow close ]
]

{ #category : #accessing }
StSpotter class >> resetHistory [
	<script>
	"
	self resetHistory
	"
	History := nil
]

{ #category : #settings }
StSpotter class >> settingsOn: aBuilder [
	<systemsettings>

	(aBuilder group: #spotter)
		parent: #tools;
		label: 'Spotter';
		description: 'Spotter configuration'.

	(aBuilder setting: #previewVisible)
		parent: #spotter;
		default: self isShowingPreview;
		target: self;
		description: 'If Spotter will show the preview panel on the right of spotter';
		label: 'Show Preview Panel'.
]

{ #category : #accessing }
StSpotter class >> showPreview [
	<script>
	
	Preview := true
]

{ #category : #private }
StSpotter class >> tips [ 

	^ Tips ifNil: [ Tips := self createTips ]
]

{ #category : #private }
StSpotter class >> windowClosed [

	spotterWindow := nil
]

{ #category : #'private actions' }
StSpotter >> activate: aPresenter [

	aPresenter activateOn: self
]

{ #category : #'private actions' }
StSpotter >> activateLink: aCandidateLink [

	self window close.
	self historize: aCandidateLink.
	aCandidateLink value doEvaluate.

]

{ #category : #private }
StSpotter >> allowUIToDraw [

	"5 is a magic number we tested is enough to allow the UI to display the spotter"
	5 timesRepeat: [ Processor yield ].

]

{ #category : #private }
StSpotter >> configureModel: aModel [ 

	aModel 
		whenSearchUpdatedDo: [ :aCandidateLink | 
			self updateResultList: aCandidateLink ];
		whenAmountChangedDo: [ :processor :amount | 
			self updateResultProcessor: processor amount: amount ];
		whenQueryStartedDo: [ self showSpinner ];
		whenQueryEndedDo: [ self hideSpinner ]
]

{ #category : #private }
StSpotter >> defaultModel [

	^ StSpotterModel new
		in: [ :this | self configureModel: this ];
		yourself
]

{ #category : #'private updating' }
StSpotter >> deferredUpdateResultList: aCandidateLink [
	| categoryPresenter presenters |

	categoryPresenter := aCandidateLink processor 
		headerCategoryFor: aCandidateLink
		on: self.
		
	(categoryPresenter includesLink: aCandidateLink) ifTrue: [ ^ self ].
	
	categoryPresenter addCandidate: aCandidateLink inSpotter: self.
	
	presenters := (categories values 
		sorted: [ :each | each order ] ascending)
		flatCollect: [ :each | each withCandidates ]. 
	
	resultList disableActivationDuring: [ 
		| index |
		index := resultList selection selectedIndex.
		resultList items: presenters.
		resultList selectIndex: index ]
]

{ #category : #'private actions' }
StSpotter >> diveInProcessor: aProcessor [

	[
		self showSpinner.
		resultList presenters: #().
		self removeAll.
		self model pushStepForProcessor: aProcessor.
		searchText text: '' 
	] schedule
]

{ #category : #'private actions' }
StSpotter >> diveOut [

	self removeAll.
	self model popStep
]

{ #category : #'private actions' }
StSpotter >> feedTip [
	
	"tip label: self class tips atRandom"
]

{ #category : #private }
StSpotter >> handleKeyEvent: anEvent [
	
	(#( 30 "arrow up" 31 "arrow down" 13 "cr") includes: anEvent keyCharacter asInteger)
		ifFalse: [ searchText takeKeyboardFocus ]
]

{ #category : #'private updating' }
StSpotter >> headerCategoryFor: aLink [

	^ categories 
		at: aLink processor
		ifAbsentPut: [ self newSpotterHeaderPresenter: aLink processor ]
]

{ #category : #'private updating' }
StSpotter >> headerCategoryUnifiedFor: aLink [

	^ categories 
		at: aLink value class
		ifAbsentPut: [ 
			(self newSpotterHeaderPresenter: aLink processor)
				type: aLink value class;
				yourself ]
]

{ #category : #'private actions' }
StSpotter >> hideSpinner [

	[
		self application defer: [ spinner hide ]
	] schedule
]

{ #category : #'private actions' }
StSpotter >> historize: aCandidateLink [

	self class historize: aCandidateLink
]

{ #category : #initialization }
StSpotter >> initialExtent [

	self flag: #TODO. "Maybe calculate coordinates?"
	^ self isShowingPreview 
		ifTrue: [ 750@450 ]
		ifFalse: [ 500@450 ]
]

{ #category : #initialization }
StSpotter >> initialize [

	mutex ifNil: [ mutex := Mutex new ].
	categories := Dictionary new.
	super initialize
]

{ #category : #initialization }
StSpotter >> initializeDialogWindow: aDialogWindowPresenter [

	aDialogWindowPresenter closeOnBackdropClick: true
]

{ #category : #initialization }
StSpotter >> initializePresenters [
	| contentsLayout |

	self layout: (SpBoxLayout newVertical
		borderWidth: 10;
		spacing: 5;
		add: (searchText := self newSearchInput) expand: false;
		add: (SpOverlayLayout new
			child: ((contentsLayout := SpBoxLayout newHorizontal)
				beHomogeneous;
				spacing: 5;
				add: (resultList := self newComponentList);
				yourself);
			addOverlay: (spinner := self newSpinner) 
				withConstraints: [ :c | c vAlignCenter; hAlignCenter ];
			yourself);
		"add: (SpBoxLayout newHorizontal
				vAlignCenter;
				add: (tip := self newLabel);
				addLast: (self newButton 
					label: 'More...';
					action: [ self feedTip ];
					yourself);
				yourself)
			expand: false;"
		yourself).

	spinner hide.

	resultList
		bindKeyCombination: Character arrowUp asKeyCombination 
			toAction: [ :target :widget :event |
				self transferFocusToSearchText.
				event wasHandled: false ];
		bindKeyCombination: Character arrowRight asKeyCombination 
			toAction: [ self tryDiveIn ];
		bindKeyCombination: Character arrowLeft asKeyCombination 
			toAction: [ self tryDiveOut ];
		whenActivatedDo: [ :selection | 
			self activate: selection selectedItem ].
	resultList eventHandler 
		whenKeyDownDo: [ :anEvent | self handleKeyEvent: anEvent ].
		
	self isShowingPreview ifTrue: [ 
		contentsLayout 	add: (previewContainer := SpBoxLayout newVertical).
		resultList whenSelectionChangedDo: [ :selection | 
			self showPreview: selection selectedItem ] ].

	self addStyle: 'stSpotter'.
	searchText 
		placeholder: 'Search...';
		addStyle: 'stSpotterSearch';
		whenTextChangedDo: [ :aString | self updateSearch: aString ];
		bindKeyCombination: Character arrowDown asKeyCombination 
			toAction: [ self transferFocusToResultList ];
		bindKeyCombination: Character arrowUp asKeyCombination 
			toAction: [ self transferFocusToResultListAtLast ].
	resultList addStyle: 'stSpotterList'.
]

{ #category : #initialization }
StSpotter >> initializeWindow: aWindowPresenter [

	super initializeWindow: aWindowPresenter.
	aWindowPresenter 
		title: 'Spotter';
		withoutDecorations;
		initialExtent: self initialExtent;
		whenOpenedDo: [ self startProcessing ];
		whenClosedDo: [ 
			self stopProcessing.
			self class windowClosed ];
		centered.

	self flag: #TODO. "This is more or less hacky, since I am using the processors that are
	relevant to the first level of spotter as the only ones, but for now it is working"
	self model activeProcessors do: [ :each | 
		each 
			installKeymappingsOn: aWindowPresenter
			onExecution: [ :aProcessor | self updateSearchFromProcessor: aProcessor ] ]
]

{ #category : #testing }
StSpotter >> isShowingPreview [

	^ self class isShowingPreview
]

{ #category : #'accessing model' }
StSpotter >> model [

	^ model ifNil: [ model := self defaultModel ]
]

{ #category : #'private factory' }
StSpotter >> newSpotterCandidateLinkPresenter: aLink [

	^ self instantiate: StSpotterCandidateLinkPresenter on: aLink
]

{ #category : #'private factory' }
StSpotter >> newSpotterHeaderPresenter: aProcessor [

	^ (self instantiate: StSpotterHeaderPresenter on: aProcessor)
		nested: self model isNested;
		whenDiveInDo: [ :processor | self diveInProcessor: processor ];
		whenDiveOutDo: [ self diveOut ];
		yourself
]

{ #category : #private }
StSpotter >> removeAll [

	"resultList presenters: #()."
	self allowUIToDraw.
	categories removeAll
]

{ #category : #'accessing model' }
StSpotter >> setModelBeforeInitialization: aSpotterModel [

	model := aSpotterModel.
	self configureModel: model
]

{ #category : #'private actions' }
StSpotter >> showPreview: aPresenter [

	previewContainer removeAll.	
	(aPresenter isNil or: [ aPresenter isHeader ]) 
		ifTrue: [ ^ self ].

	aPresenter model value spotterPreview 
		ifNotNil: [ :previewPresenter | 
			previewContainer add: (previewPresenter 
				owner: self; 
				yourself) ]
]

{ #category : #'private actions' }
StSpotter >> showSpinner [

	spinner isVisible ifTrue: [ ^ self ].
	[
		self application defer: [ spinner show ]
	] schedule
]

{ #category : #private }
StSpotter >> startProcessing [
	"Start the process."

	[ 
		self allowUIToDraw.
		self model startProcessing 
	] schedule
]

{ #category : #private }
StSpotter >> stopProcessing [
	"Start the process."

	self model stopProcessing
]

{ #category : #private }
StSpotter >> transferFocusToResultList [

	self transferFocusToResultListAt: 1
]

{ #category : #private }
StSpotter >> transferFocusToResultListAt: index [

	resultList items isEmptyOrNil ifTrue: [ ^ self ].
	
	resultList 
		selectIndex: index;
		takeKeyboardFocus
]

{ #category : #private }
StSpotter >> transferFocusToResultListAtLast [ 

	self transferFocusToResultListAt: (resultList presenters size)
]

{ #category : #private }
StSpotter >> transferFocusToSearchText [

	(resultList items notEmpty and: [ resultList selection selectedIndex = 1 ])
		ifTrue: [ searchText takeKeyboardFocus ].
		
	
]

{ #category : #'private actions' }
StSpotter >> tryDiveIn [
	| item |
	
	self model isNested ifTrue: [ ^ self ].
	item := resultList selectedItem.
	item ifNil: [ ^ self ].
	item isHeader ifFalse: [ ^ self ].
	item diveIn
]

{ #category : #'private actions' }
StSpotter >> tryDiveOut [
	| item |
	
	self model isNested ifFalse: [ ^ self ].
	item := resultList selectedItem.
	item ifNil: [ ^ self ].
	item isHeader ifFalse: [ ^ self ].
	item diveOut
]

{ #category : #initialization }
StSpotter >> updatePresenter [

	self feedTip
]

{ #category : #'private updating' }
StSpotter >> updateResultList: aCandidateLink [

	[
		self application defer: [
			mutex critical: [
				self deferredUpdateResultList: aCandidateLink ] ]
	] schedule
]

{ #category : #'private updating' }
StSpotter >> updateResultProcessor: processor amount: amount [

	self application defer: [ 
		categories 
			at: processor 
			ifPresent: [ :aPresenter | aPresenter finalAmount: amount ] ]
]

{ #category : #'private updating' }
StSpotter >> updateSearch: aString [
	
	self removeAll.
	self model asyncProcess: aString
]

{ #category : #'private updating' }
StSpotter >> updateSearchFromProcessor: aProcessor [

	searchText text: aProcessor query textInSearchBar.
	searchText cursorPositionIndex: searchText text size + 1.
]
