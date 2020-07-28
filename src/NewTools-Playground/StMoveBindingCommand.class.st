Class {
	#name : #StMoveBindingCommand,
	#superclass : #StCommand,
	#category : #'NewTools-Playground-Command'
}

{ #category : #default }
StMoveBindingCommand class >> defaultDescription [

	^ 'Switch binding scope from local to shared and viceversa.'
]

{ #category : #default }
StMoveBindingCommand class >> defaultName [

	^ 'Move to shared/local scope'
]

{ #category : #accessing }
StMoveBindingCommand class >> moveToLocalName [

	^ 'Make binding local'
]

{ #category : #accessing }
StMoveBindingCommand class >> moveToSharedName [

	^ 'Make binding shared'
]

{ #category : #executing }
StMoveBindingCommand >> execute [
	
	context toggleSelectedBindingScope
]

{ #category : #accessing }
StMoveBindingCommand >> name [

	context ifNil: [ ^ super name ].

	^ context selectedBindingIsLocal
		ifTrue: [ self class moveToSharedName ]
		ifFalse: [ self class moveToLocalName ]
]
