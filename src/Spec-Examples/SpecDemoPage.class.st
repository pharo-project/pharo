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

{ #category : #accessing }
SpecDemoPage class >> availablePages [
	^ self allSubclasses sort: #priority ascending
]

{ #category : #specs }
SpecDemoPage class >> defaultSpec [
	^ SpecBoxLayout newVertical
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
	^ NotebookPage
		title: 'Code'
		icon: (self iconNamed: #changeUpdate)
		provider: [ self newText
				text: (self codeFor: self pageClass);
				yourself ]
]

{ #category : #initialization }
SpecDemoPage >> exampleTab [
	^ NotebookPage title: 'Example' icon: (self iconNamed: #smallPaint) provider: [ self instantiate: self pageClass ]
]

{ #category : #initialization }
SpecDemoPage >> initializeWidgets [

	tabManager := self newNotebook.
	"tabManager whenTabSelected: [ self updateTitle ]."
	tabManager
		addPage: self exampleTab;
		addPage: self codeTab.
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
