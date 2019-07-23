Class {
	#name : #CBBrowseCriticCommand,
	#superclass : #CmCommand,
	#category : #'Tool-CriticBrowser-Commands'
}

{ #category : #hooks }
CBBrowseCriticCommand >> execute [
	CBBrowseCriticAction runOn: self context
]

{ #category : #initialization }
CBBrowseCriticCommand >> initialize [
	super initialize.
	self
		basicName: 'Browse';
		basicDescription: 'Open a browser on the selected critic.'
]
