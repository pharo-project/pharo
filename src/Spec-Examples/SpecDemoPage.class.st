"
A superclass for particular Spec demo pages, that need to define a page name and priority in the list of demos.
"
Class {
	#name : #SpecDemoPage,
	#superclass : #ComposablePresenter,
	#instVars : [
		'tabManager'
	],
	#category : #'Spec-Examples-Demo'
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
SpecDemoPage >> codeFor: aClass [
	^ TonelWriter sourceCodeOf: aClass
]

{ #category : #initialization }
SpecDemoPage >> codeTab [
	^ self newTab
		label: 'Code';
		icon: (self iconNamed: #changeUpdate);
		presenter:
			(self newCode
				text: (self codeFor: self pageClass);
				yourself);
		yourself
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
	^ self newTab
		label: 'Comment';
		icon: (self iconNamed: #helpIcon);
		presenter:
			(self newText
				text: (self commentFor: self pageClass);
				yourself);
		yourself
]

{ #category : #initialization }
SpecDemoPage >> exampleTab [
	^ self newTab
		label: 'Example';
		icon: (self iconNamed: #smallPaint);
		presenter: (self instantiate: self pageClass);
		yourself
]

{ #category : #initialization }
SpecDemoPage >> initializeWidgets [

	tabManager := self newTabManager.
	"tabManager whenTabSelected: [ self updateTitle ]."
	tabManager
		addTab: self exampleTab;
		addTab: self commentTab;
		addTab: self codeTab.
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
