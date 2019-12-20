Class {
	#name : #GLMExplicitBrowserCopyTest,
	#superclass : #TestCase,
	#instVars : [
		'browser',
		'copiedBrowser'
	],
	#category : #'Glamour-Tests-Core'
}

{ #category : #running }
GLMExplicitBrowserCopyTest >> setUp [
	| pane1 pane2 transmission1 transmission2 | 
	super setUp.
	browser := GLMExplicitBrowser new. 
	
	pane1 := browser addNewPane name: 'pane1'. 
	pane2 := browser addNewPane name: 'pane2'. 
	transmission1 := GLMTransmission new
		addActiveOrigin: (pane2 port: #selection); 
		destination: (GLMPresentationBoundPort new name: #selection; presentation: browser; yourself); 
		yourself.  
	browser addTransmission: transmission1. 
	transmission2 := GLMTransmission new 
		ensureReplacePresentationsStrategy;
		addActiveOrigin: (pane1 port: #selection); 
		destination: (pane2 port: #entity); 
		addPresentation: (GLMPresentation new title: 'presentation1');
		yourself.  
	browser addTransmission: transmission2. 
	copiedBrowser := browser copy
]

{ #category : #tests }
GLMExplicitBrowserCopyTest >> testEquality [
	self assert: (copiedBrowser = browser) not
]

{ #category : #tests }
GLMExplicitBrowserCopyTest >> testPanes [
	self assert: copiedBrowser panes size equals: 2.
	self assert: copiedBrowser panes first ~~ browser panes first.
	self assert: copiedBrowser panes last ~~ browser panes last.
	self assert: copiedBrowser panes first browser identicalTo: copiedBrowser.
	self assert: copiedBrowser panes last browser identicalTo: copiedBrowser.
	self assert: copiedBrowser panes first ports size equals: 1.
	self assert: copiedBrowser panes first ports first name equals: #selection.
	self assert: copiedBrowser panes first ports first pane identicalTo: copiedBrowser panes first.
	self assert: copiedBrowser panes last ports size equals: 2.
	self assert: copiedBrowser panes last ports first name equals: #selection.
	self assert: copiedBrowser panes last ports last name equals: #entity
]

{ #category : #tests }
GLMExplicitBrowserCopyTest >> testTrasmissions [
	self assert: copiedBrowser transmissions size equals: 2.
	self assert: copiedBrowser transmissions first ~~ browser transmissions first.
	self assert: copiedBrowser transmissions last ~~ browser transmissions last.
	self assert: (copiedBrowser transmissions first destination pane browser isKindOf: GLMNoBrowser).
	self assert: copiedBrowser transmissions last destination pane browser identicalTo: copiedBrowser.
	self assert: copiedBrowser transmissions first origins first pane browser identicalTo: copiedBrowser.
	self assert: copiedBrowser transmissions last origins first pane browser identicalTo: copiedBrowser
]
