Class {
	#name : #SpecProgressBarDemo,
	#superclass : #ComposablePresenter,
	#instVars : [
		'indeterminated',
		'fixed',
		'withProgressionBlock',
		'withText',
		'label'
	],
	#category : #'Spec-Examples-Demo-Loading'
}

{ #category : #specs }
SpecProgressBarDemo class >> defaultSpec [
	^ SpecLayout composed
		newColumn: [ :c | 
			c
				newRow: #indeterminated height: 6;
				addSpacer;
				newRow: #fixed height: 10;
				addSpacer;
				newRow: #withProgressionBlock height: 20;
				addSpacer;
				newRow: [ :r | 
					r
						newColumn: #withText;
						newColumn: #label width: 40 ]
					height: 40;
				newRow: [ :r |  ] ];
		yourself
]

{ #category : #accessing }
SpecProgressBarDemo >> fixed [
	^ fixed
]

{ #category : #accessing }
SpecProgressBarDemo >> fixed: anObject [
	fixed := anObject
]

{ #category : #accessing }
SpecProgressBarDemo >> indeterminated [
	^ indeterminated
]

{ #category : #accessing }
SpecProgressBarDemo >> indeterminated: anObject [
	indeterminated := anObject
]

{ #category : #initialization }
SpecProgressBarDemo >> initializePresenter [
	withText whenValueChangedDo: [ :value | label label: (value * 100) asInteger asString , '%' ]
]

{ #category : #initialization }
SpecProgressBarDemo >> initializeWidgets [
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
SpecProgressBarDemo >> label [
	^ label
]

{ #category : #accessing }
SpecProgressBarDemo >> withProgressionBlock [
	^ withProgressionBlock
]

{ #category : #accessing }
SpecProgressBarDemo >> withProgressionBlock: anObject [
	withProgressionBlock := anObject
]

{ #category : #accessing }
SpecProgressBarDemo >> withText [
	^ withText
]

{ #category : #accessing }
SpecProgressBarDemo >> withText: anObject [
	withText := anObject
]
