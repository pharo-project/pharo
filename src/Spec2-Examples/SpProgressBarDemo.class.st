"
A demo for progress bar usage

   self example
"
Class {
	#name : #SpProgressBarDemo,
	#superclass : #SpPresenter,
	#instVars : [
		'indeterminated',
		'fixed',
		'withProgressionBlock',
		'withText',
		'label'
	],
	#category : #'Spec2-Examples-Demo-Loading'
}

{ #category : #specs }
SpProgressBarDemo class >> defaultSpec [
	^ SpBoxLayout newVertical
		add: #indeterminated withConstraints: [ :constraints | constraints height: 6 ];
		add: 'Indeterminated';
		add: #fixed withConstraints: [ :constraints | constraints height: 10 ];
		add: 'Fixed';
		add: #withProgressionBlock withConstraints: [ :constraints | constraints height: 20 ];
		add: 'With progression block';
		add:
			(SpBoxLayout newHorizontal
				add: #withText;
				add: #label withConstraints: [ :constraints | constraints width: 40 ];
				yourself)
			withConstraints: [ :constraints | constraints height: 40 ];
		add: 'With label';
		yourself
]

{ #category : #examples }
SpProgressBarDemo class >> example [
	<sampleInstance>
	
	^ self new openWithSpec
]

{ #category : #initialization }
SpProgressBarDemo >> connectPresenters [
	withText whenValueChangedDo: [ :value | label label: (value * 100) asInteger asString , '%' ]
]

{ #category : #accessing }
SpProgressBarDemo >> fixed [
	^ fixed
]

{ #category : #accessing }
SpProgressBarDemo >> fixed: anObject [
	fixed := anObject
]

{ #category : #accessing }
SpProgressBarDemo >> indeterminated [
	^ indeterminated
]

{ #category : #accessing }
SpProgressBarDemo >> indeterminated: anObject [
	indeterminated := anObject
]

{ #category : #initialization }
SpProgressBarDemo >> initializePresenters [
	| progress progress2 |
	indeterminated := self newProgressBar.
	fixed := self newProgressBar.
	withProgressionBlock := self newProgressBar.
	withText := self newProgressBar.
	label := self newLabel.

	indeterminated indeterminate.
	
	fixed fixedPercentage: 30.

	progress := 0.
	withProgressionBlock progress: [ progress := progress + 0.01 ] every: 0.5 second.
	
	progress2 := 0.
	withText progress: [ progress2 := progress2 + 0.01 ] every: 0.1 second.
	label label: 'This is a progress bar'
]

{ #category : #accessing }
SpProgressBarDemo >> label [
	^ label
]

{ #category : #accessing }
SpProgressBarDemo >> withProgressionBlock [
	^ withProgressionBlock
]

{ #category : #accessing }
SpProgressBarDemo >> withProgressionBlock: anObject [
	withProgressionBlock := anObject
]

{ #category : #accessing }
SpProgressBarDemo >> withText [
	^ withText
]

{ #category : #accessing }
SpProgressBarDemo >> withText: anObject [
	withText := anObject
]
