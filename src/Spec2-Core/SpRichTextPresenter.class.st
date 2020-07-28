"
@@TODO

!! Transmissions
- #inputTextPort (default)
- #outputTextChangedPort (default)
"
Class {
	#name : #SpRichTextPresenter,
	#superclass : #SpTextPresenter,
	#instVars : [
		'#textStyler => SpObservableSlot',
		'#model'
	],
	#category : #'Spec2-Core-Widgets-Text'
}

{ #category : #specs }
SpRichTextPresenter class >> adapterName [

	^ #RichTextAdapter
]

{ #category : #examples }
SpRichTextPresenter class >> example [
	<sampleInstance>

	^ self new
	 openWithSpec;
		text:
			'some method: 42'

]

{ #category : #specs }
SpRichTextPresenter class >> title [

	^ 'Rich Text'
]

{ #category : #accessing }
SpRichTextPresenter >> defaultTextStyler [
	self subclassResponsibility 
]

{ #category : #rendering }
SpRichTextPresenter >> formatString [
	^ self textStyler value writeFormatString: self model
]

{ #category : #rendering }
SpRichTextPresenter >> formatString: formatString [
	model := self textStyler value parseFormatString: formatString 
]

{ #category : #initialization }
SpRichTextPresenter >> initialize [ 

	super initialize
]

{ #category : #rendering }
SpRichTextPresenter >> model [
	^  model
]

{ #category : #'as yet unclassified' }
SpRichTextPresenter >> plaintext [ 

	^ nil
	"^ self textStyler value writePlaintextString: self model"
]

{ #category : #rendering }
SpRichTextPresenter >> renderTree [
	
	^ nil
	"^ SpRenderTreeBuilderVisitor new 
			styleProvider: self styleAttributes;
			render: self model" 
]

{ #category : #initialization }
SpRichTextPresenter >> styleAttributes [
	^ self textStyler value styles
]

{ #category : #accessing }
SpRichTextPresenter >> textStyler [

	^ textStyler ifNil: [ 
		textStyler := self defaultTextStyler ]
]

{ #category : #accessing }
SpRichTextPresenter >> textStyler: aTextStyler [

	textStyler := aTextStyler
]

{ #category : #'api-events' }
SpRichTextPresenter >> whenTextStylerChangedDo: aBlock [

	self property: #textStyler whenChangedDo: aBlock
]
