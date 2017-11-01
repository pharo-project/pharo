Class {
	#name : #GLMMorphicFinderRenderer,
	#superclass : #GLMMorphicBrowserRenderer,
	#instVars : [
		'container'
	],
	#category : 'Glamour-Morphic-Renderer'
}

{ #category : #rendering }
GLMMorphicFinderRenderer >> actOnPaneAdded: ann [
	self subclassResponsibility
]

{ #category : #rendering }
GLMMorphicFinderRenderer >> actOnPaneRemoved: ann [
	self subclassResponsibility
]

{ #category : #rendering }
GLMMorphicFinderRenderer >> actOnPaneReplaced: ann [
	self subclassResponsibility
]

{ #category : #rendering }
GLMMorphicFinderRenderer >> registerAnnouncementsFor: aBrowser [
	aBrowser when: GLMPaneRemoved send: #actOnPaneRemoved: to: self.
	aBrowser when: GLMPaneAdded send: #actOnPaneAdded: to: self.
	aBrowser when: GLMPaneReplaced send: #actOnPaneReplaced: to: self.
	aBrowser when: GLMBrowserClosed send: #actOnBrowserClosed: to: self
]

{ #category : #rendering }
GLMMorphicFinderRenderer >> render: aBrowser [
	^ self subclassResponsibility
]
