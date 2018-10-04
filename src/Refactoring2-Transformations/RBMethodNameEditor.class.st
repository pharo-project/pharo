"
I am an editor to set the name of a method from a given collection of arguments. Optionally, one can add or remove arguments.
"
Class {
	#name : #RBMethodNameEditor,
	#superclass : #MethodNameEditor,
	#instVars : [
		'labelGroup'
	],
	#category : #'Refactoring2-Transformations-Utilities'
}

{ #category : #actions }
RBMethodNameEditor >> add [

	| newArguments |
	newArguments := self argumentList.
	newArguments add: ('arg', (newArguments size + 1) asString).
	
	self methodName arguments: newArguments.
	self update
]

{ #category : #accessing }
RBMethodNameEditor >> argumentList [

	^ self methodName
		ifNil: [ OrderedCollection new ]
		ifNotNil: [ :method | method arguments copy ]
]

{ #category : #testing }
RBMethodNameEditor >> isOkEnabled [

	^ self methodName isNotNil
		and: [ self methodName isValid ]
]

{ #category : #morphic }
RBMethodNameEditor >> newContentMorph [

	^ labelGroup := (self newRow: {
		self newLabelGroup: {
			'Selector:' -> (selectorField := 
			(self newTextEntryFor: self getText: #selector setText: #selector: help: nil)
				autoAccept: true;
				on: #keyStroke send: #value:value:
				to: [:key :morph | key keyCharacter = Character cr ifTrue: [ self ok. true]. false];
				hResizing: #spaceFill;
				yourself).
				
			'Arguments:' -> (self newRow: {
				(self newColumn: {
					self newButtonFor: self action: #add label: '+'  help: nil.
					self newButtonFor: self action: #remove label: '-'  help: nil })
					hResizing: #shrinkWrap.
				(self newListFor: self list: #argumentList
					selected: #argumentIndex
					changeSelected: #argumentIndex: help: nil)
					hResizing: #spaceFill;
					yourself. }).
					
			'Preview:' -> (labelMorph := self newLabel: self methodName printString) } } )
 		minWidth: 400;
		hResizing: #spaceFill;
		yourself
]

{ #category : #actions }
RBMethodNameEditor >> remove [

	| newArguments |
	newArguments := self argumentList.
	newArguments removeAt: self argumentIndex.
	
	self methodName arguments: newArguments.
	self update
]

{ #category : #accessing }
RBMethodNameEditor >> selector [

	^ self methodName ifNotNil: [ :method | method selector ]
]

{ #category : #accessing }
RBMethodNameEditor >> selector: aString [

	super selector: aString.
	(labelGroup extent x > self extent x) ifTrue: [ 
		self extent: ((labelGroup extent x+20) @ labelGroup extent y) ]
	
	
]
