"
I am base for Spec Standard Dialogs.
Standard dialogs are available to provide simple I/O to users.
"
Class {
	#name : #SpDialogPresenter,
	#superclass : #SpPresenter,
	#instVars : [
		'#parentWindow => WeakSlot',
		'#acceptAction',
		'#cancelAction'
	],
	#category : #'Spec2-Dialogs'
}

{ #category : #documentation }
SpDialogPresenter class >> addDocumentSection: aBuilder label: label methods: methods [
	
	methods ifEmpty: [ ^ self ].
	
	aBuilder newLine.
	aBuilder header: [ :builder | builder text: label ] withLevel: 2.
	aBuilder unorderedListDuring: [  
		(methods sorted: #selector ascending) do: [ :each |
			aBuilder item: [
				aBuilder monospace: (each methodClass name, '>>#', each selector) ] ] ]
]

{ #category : #documentation }
SpDialogPresenter class >> addDocumentSectionExampleCode: aBuilder [
	| exampleCode |

	exampleCode := self documentExampleCode. 
	exampleCode ifNil: [ ^ self ].
	
	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Example code' ] withLevel: 2.
	aBuilder newLine.
	aBuilder codeblock: exampleCode
]

{ #category : #documentation }
SpDialogPresenter class >> addDocumentSectionFactoryMethod: aBuilder [
	| selector |

	selector := self documentFactoryMethodSelector. 
	selector ifNil: [ ^ self ].
	 
	aBuilder newLine.
	aBuilder header: [ :builder | builder text: 'Factory method' ] withLevel: 2.
	aBuilder newLine.
	aBuilder text: 'You can open '. 
	aBuilder monospace: self name. 
	aBuilder text: ' dialogs in your presenters by sending '.
	aBuilder monospace: 'SpApplication>>#', selector.
	aBuilder text: '.'
]

{ #category : #documentation }
SpDialogPresenter class >> buildMicroDownUsing: aBuilder withComment: aString [
	
	super buildMicroDownUsing: aBuilder withComment: aString.
	"Example code"
	self addDocumentSectionExampleCode: aBuilder.
	"Factory method"
	self addDocumentSectionFactoryMethod: aBuilder.
	"Sections"
	self documentSections keysAndValuesDo: [ :label :methods |
		self addDocumentSection: aBuilder label: label methods: methods ].
	"Hierarchy"
	"self addDocumentSectionHierarchy: aBuilder."

]

{ #category : #accessing }
SpDialogPresenter class >> defaultAcceptLabel [

	^ 'Accept'
]

{ #category : #accessing }
SpDialogPresenter class >> defaultCancelLabel [

	^ 'Cancel'
]

{ #category : #documentation }
SpDialogPresenter class >> documentExampleCode [
	| exampleMethod |
	
	exampleMethod := self class methods 
		detect: [ :each | 
			(each protocol = #example) 
			and: [ each selector = #exampleModal ] ]
		ifNone: [ ^ nil ].
			
	^ (exampleMethod sourceCode lines 
		allButFirst 	"Remove method name"
		reject: [ :each | each trimLeft beginsWith: '<' ]) "Remove pragmas"
		asStringWithCr
		trimmed
]

{ #category : #documentation }
SpDialogPresenter class >> documentFactoryMethodSelector [
	
	^ nil
]

{ #category : #documentation }
SpDialogPresenter class >> documentSections [
		
	^ OrderedDictionary newFromPairs: {
		'API Show Methods'. self methods select: [ :each | each protocol = #'api-showing' ].
		'API Methods'. self methods select: [ :each | each protocol = #'api' ].
		'Testing Methods'. self methods select: [ :each | each protocol = #'testing' ].
		'Events'. self methods select: [ :each | each protocol = #'api-events' ].
	}
]

{ #category : #'instance creation' }
SpDialogPresenter class >> open [
	<script>
	
	^ self new openWithSpec
]

{ #category : #'instance creation' }
SpDialogPresenter class >> openDialog [
	<script>
	
	^ self new openDialogWithSpec
]

{ #category : #private }
SpDialogPresenter >> accept [

	self window 
		beOk; 
		triggerOkAction;
		close
]

{ #category : #private }
SpDialogPresenter >> cancel [

	self window 
		beCancel; 
		triggerCancelAction;
		close
]

{ #category : #initialization }
SpDialogPresenter >> initializeDialogWindow: aDialogWindowPresenter [

	aDialogWindowPresenter 
		okAction: acceptAction;
		cancelAction: cancelAction.
	
	self parentWindow 
		ifNotNil: [ :w | aDialogWindowPresenter centeredRelativeTo: w ] 
		ifNil: [ aDialogWindowPresenter centered ].
]

{ #category : #'api-events' }
SpDialogPresenter >> onAccept: aBlock [
	"When the dialog is accepted, it executes this action before close.
	 `aBlock` receives one argument (this dialog)"

	acceptAction := aBlock
]

{ #category : #'api-events' }
SpDialogPresenter >> onCancel: aBlock [
	"When the dialog is cancelled, it executes this action before close.
	 `aBlock` receives one argument (this dialog)"

	cancelAction := aBlock
]

{ #category : #'api-showing' }
SpDialogPresenter >> openDialogWithSpec [
	"Open dialog in a non-modal way."

	^ super openDialogWithSpec
]

{ #category : #'api-showing' }
SpDialogPresenter >> openDialogWithSpecParent: aWindow [
	"Open dialog in a non-modal way, centering on parent window. "
	
	parentWindow := aWindow.
	^ self openDialogWithSpec
]

{ #category : #'api-showing' }
SpDialogPresenter >> openModalWithSpec [
	"Open dialog in a modal way. "

	^ super openModalWithSpec
]

{ #category : #'api-showing' }
SpDialogPresenter >> openModalWithSpecParent: aWindow [
	"Open dialog in a modal way, centering on parent window. "

	parentWindow := aWindow.
	^ self openModalWithSpec
]

{ #category : #accessing }
SpDialogPresenter >> parentWindow [

	^ parentWindow
]
