"
A superclass for particular Spec demo pages, that need to define a page name and priority in the list of demos.
"
Class {
	#name : #SpecDemoPage,
	#superclass : #ComposablePresenter,
	#instVars : [
		'tabManager'
	],
	#category : #'Spec-Examples-Demo-Support'
}

{ #category : #specs }
SpecDemoPage class >> defaultSpec [
	<spec>
	
	^ SpecLayout composed
		add: #tabManager;
		yourself
]

{ #category : #initialization }
SpecDemoPage class >> pageName [

	self subclassResponsibility 

]

{ #category : #initialization }
SpecDemoPage class >> priority [

	^ 999
]

{ #category : #initialization }
SpecDemoPage >> commentFor: aClass [

	^ String streamContents: [ :s |
		s nextPutAll: aClass name; cr.
		s nextPutAll: '--------------------------------------------'; cr.
		s cr.
		s nextPutAll: aClass comment ]
		
]

{ #category : #initialization }
SpecDemoPage >> commentTab [
	| tab commentInput |
	tab := self newTab.
	tab
		label: 'Comment';
		icon: (tab iconNamed: #helpIcon);
		presenter: (commentInput := self newText).
	commentInput text: (self commentFor: self pageClass).
	^ tab
]

{ #category : #initialization }
SpecDemoPage >> exampleTab [
	| tab |
	tab := self newTab.
	tab
		label: 'Example';
		icon: (tab iconNamed: #smallPaint);
		presenter: (self instantiate: self pageClass).
	^ tab
]

{ #category : #initialization }
SpecDemoPage >> initializeWidgets [

	tabManager := self newTabManager.
	"tabManager whenTabSelected: [ self updateTitle ]."
	tabManager
		addTab: self exampleTab;
		addTab: self commentTab.
]

{ #category : #initialization }
SpecDemoPage >> pageClass [

	self subclassResponsibility 

]

{ #category : #accessing }
SpecDemoPage >> tabManager [
	^ tabManager
]

{ #category : #accessing }
SpecDemoPage >> tabManager: anObject [
	tabManager := anObject
]
