"
Some examples of the action of a pretty printing option 
"
Class {
	#name : #BIFormatSettingsExample,
	#superclass : #Object,
	#category : #'BlueInk-Core-Examples'
}

{ #category : #prettyPrintOptionRelated }
BIFormatSettingsExample >> cascadedMessageInsideParenthesesExample [
	"option on :
	(self bar)
			foo;
			foo;
			foo
			
	option off :
	self bar
			foo;
			foo;
			foo"
]

{ #category : #prettyPrintOptionRelated }
BIFormatSettingsExample >> keepBlockInMessageExample [
	"option on:
	
	self foo
	or: [ 
	self bar
	ifTrue: [ true ] ]
	
	option off:
	
	self foo
	or:
	[ 
	self bar
	ifTrue: [ true ] ]"

	
]

{ #category : #prettyPrintOptionRelated }
BIFormatSettingsExample >> lineUpBlockBracketsExample [
	"option on:
	self foo
	or: [ 
	self bar
	ifTrue: [ true ] ]
	
	option off: 
	self foo
		or: [ 
			self bar
				ifTrue: [ true ]
			]"
]

{ #category : #prettyPrintOptionRelated }
BIFormatSettingsExample >> newLineAfterCascadeExample [
	"option on:
		self
			foo;
			foo;
			foo
			
	option off: 
	self
		foo; foo; foo"
]

{ #category : #prettyPrintOptionRelated }
BIFormatSettingsExample >> newLineBeforeFirstCascadeExample [
	"option on:
		self
			foo;
			foo;
			foo	
				
	option off: 
		self foo;
			foo;
			foo"
]

{ #category : #prettyPrintOptionRelated }
BIFormatSettingsExample >> newLineBeforeFirstKeywordExample [
	"option on: 
		self
		foo
		
	option off:
		self foo"
]
