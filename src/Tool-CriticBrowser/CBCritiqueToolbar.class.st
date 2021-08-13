"
I am the toolbar for displayed critiques in the critique browser.

* If you select 'Browse', I will open system browser with the location of the critique.
* If you select 'Mark as wrong', I will put in grey color the selected critique and it will be saved as false positive in cache.
* If you select 'Transfrom', I will open changes browser presenter if the selected critique has the automatically resolve option.

I interact with CBSingleCodeCritiqueResultList and it's displayed critiques. 

You can open me whit:
`CBCritiqueToolbar open`.

"
Class {
	#name : #CBCritiqueToolbar,
	#superclass : #SpPresenter,
	#instVars : [
		'criticHolder',
		'browseButton',
		'falsePositiveButton',
		'transformButton',
		'selectDiffButton',
		'cache',
		'ruleHolder'
	],
	#category : #'Tool-CriticBrowser-Parts'
}

{ #category : #specs }
CBCritiqueToolbar class >> defaultSpec [
	
	<spec>
	^ SpBoxLayout newLeftToRight
		  add: #browseButton;
		  add: #falsePositiveButton;
			  add: #transformButton;
		  "add: #selectDiffButton height: 25;"yourself
]

{ #category : #specs }
CBCritiqueToolbar class >> open [

	<example>
	^ self new openWithSpec
]

{ #category : #specs }
CBCritiqueToolbar class >> title [
	
	^ 'Critique Toolbar'
]

{ #category : #accessing }
CBCritiqueToolbar >> addCriticToFalsePositive [

	criticHolder value ifNotNil: [ :item |
		cache addFalsePositive: item forRule: ruleHolder value ]
]

{ #category : #accessing }
CBCritiqueToolbar >> browseCritique [
	
	criticHolder value ifNotNil: [ :elem | 
		^ elem sourceAnchor entity browse ]
]

{ #category : #accessing }
CBCritiqueToolbar >> cache: aCache [

	cache := aCache
]

{ #category : #accessing }
CBCritiqueToolbar >> criticHolder [

	^ criticHolder
]

{ #category : #accessing }
CBCritiqueToolbar >> criticHolder: anObject [

	criticHolder := anObject
]

{ #category : #accessing }
CBCritiqueToolbar >> critique: aCritique [

	criticHolder value: aCritique.
	transformButton visibleIf:
		(aCritique isNotNil and: [ aCritique providesChange ])
]

{ #category : #accessing }
CBCritiqueToolbar >> falsePositiveButton [

	^ falsePositiveButton
]

{ #category : #initialization }
CBCritiqueToolbar >> initialize [

	super initialize.
	criticHolder := nil asValueHolder.
	ruleHolder := nil asValueHolder.
	browseButton := self newBrowseButton.
	transformButton := self newTransformButton.
	falsePositiveButton := self newFalsePositiveButton.
	self setFocusOrder
]

{ #category : #accessing }
CBCritiqueToolbar >> newBrowseButton [

	^ self newButton
		  label: 'Browse';
		  icon: (self iconNamed: #nautilus);
		  help: 'Explore and modify in System Browser';
		  action: [ self browseCritique ];
		  yourself
]

{ #category : #accessing }
CBCritiqueToolbar >> newFalsePositiveButton [

	^ self newButton
		  label: 'Mark as wrong';
		  icon: (self iconNamed: #stop);
		  action: [ self addCriticToFalsePositive ];
		  yourself
]

{ #category : #accessing }
CBCritiqueToolbar >> newTransformButton [

	^ self newButton
		  label: 'Transform';
		  icon: (self iconNamed: #repair);
		  help: 'Apply changes to solve issue';
		  action: [ self transformCritique ];
		  yourself
]

{ #category : #accessing }
CBCritiqueToolbar >> rule: aRule [

	ruleHolder value: aRule
]

{ #category : #accessing }
CBCritiqueToolbar >> ruleHolder [

	^ ruleHolder
]

{ #category : #accessing }
CBCritiqueToolbar >> ruleHolder: anObject [

	ruleHolder := anObject
]

{ #category : #accessing }
CBCritiqueToolbar >> setFocusOrder [

	self focusOrder
		add: browseButton;
		add: falsePositiveButton;
		add: transformButton;
		add: selectDiffButton
]

{ #category : #accessing }
CBCritiqueToolbar >> transformButton [

	^ transformButton
]

{ #category : #accessing }
CBCritiqueToolbar >> transformCritique [

	criticHolder value ifNotNil: [ :critique | 
		(CBRewriteCritiqueChangesBrowser changes: { critique change })
			  open ]
]
