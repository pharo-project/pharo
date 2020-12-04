Class {
	#name : #StRemoveBindingCommand,
	#superclass : #CmCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #accessing }
StRemoveBindingCommand >> application [

	^ StPharoApplication current
]

{ #category : #converting }
StRemoveBindingCommand >> asSpecCommand [ 
	
	^ super asSpecCommand 
		iconProvider: self application;
		iconName: #smallDelete;
		shortcutKey: $x control unix | $x control win | $x command mac;
		yourself
]

{ #category : #executing }
StRemoveBindingCommand >> execute [
	
	context removeSelectedBinding
]

{ #category : #initialization }
StRemoveBindingCommand >> initialize [
	super initialize.
	self
		name: 'Remove binding';
		description: 'Remove current binding.'
]
