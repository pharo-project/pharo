Class {
	#name : #SpRichTextPresenter,
	#superclass : #SpTextPresenter,
	#instVars : [
		'#textStyler => SpObservableSlot'
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

{ #category : #initialization }
SpRichTextPresenter >> initialize [ 

	super initialize
]

{ #category : #accessing }
SpRichTextPresenter >> textStyler [

	^ textStyler
]

{ #category : #accessing }
SpRichTextPresenter >> textStyler: aTextStyler [

	textStyler := aTextStyler
]

{ #category : #'api-events' }
SpRichTextPresenter >> whenTextStylerChangedDo: aBlock [

	self property: #textStyler whenChangedDo: aBlock
]
