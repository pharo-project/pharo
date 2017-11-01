"
I am a morphic item used to show ocompletion entries. 
"
Class {
	#name : #NOCMenuMorph,
	#superclass : #NECMenuMorph,
	#category : 'NECompletion-View'
}

{ #category : #accessing }
NOCMenuMorph >> height [
	^ 7
]

{ #category : #actions }
NOCMenuMorph >> narrowCompletion [
	| model |
	
	self selected: 0.
	firstVisible := 1.
	model := context model.
	model narrowWith: context completionToken.
	model notEmpty ifTrue: [self selected: 1].
	model entries size < self selected ifTrue: [self selected: model entries size].
	self show.
	^ true
]
