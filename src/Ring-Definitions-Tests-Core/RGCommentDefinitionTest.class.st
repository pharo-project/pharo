"
SUnit tests for comment definitions
"
Class {
	#name : #RGCommentDefinitionTest,
	#superclass : #TestCase,
	#category : #'Ring-Definitions-Tests-Core-Base'
}

{ #category : #testing }
RGCommentDefinitionTest >> testActiveComment [
	| rgComment |
	rgComment := RGCommentDefinition realClass: RGClassDefinition.
	self assert: rgComment isActive.
	self assert: rgComment sourcePointer isNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil.
	rgComment content: ''.
	"it always reads from the organization and the previous changes was not commited"
	self assert: rgComment content ~= ''.

	rgComment fromActiveToPassive.
	self assert: rgComment isPassive.
	self assert: rgComment sourcePointer isNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil.
	rgComment content: ''.
	self assert: rgComment content equals: ''.

	rgComment := RGCommentDefinition realClass: RGClassDefinition.
	rgComment fromActiveToHistorical.
	self assert: rgComment isHistorical.
	self assert: rgComment sourcePointer notNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil.
	rgComment content: ''.
	"it always reads from the source file and the previous changes was not commited"
	self assert: rgComment content ~= ''
]

{ #category : #testing }
RGCommentDefinitionTest >> testCommentFromSourceFile [
	| rgComment |
	
	rgComment := (RGCommentDefinition realClass: RGClassDefinition) asHistorical.
	self assert: rgComment isHistorical.
	self assert: rgComment sourcePointer notNil.
	self assert: rgComment content notNil.
	self assert: rgComment stamp notNil
]

{ #category : #testing }
RGCommentDefinitionTest >> testCommentWithoutAuthor [
	| newComment |
	
	newComment:= RGCommentDefinition new
						content: 'This is a comment for test';
						stamp: '3/22/2011 14:51';
						yourself.
	
	self assert: (newComment hasAuthor not).
]

{ #category : #testing }
RGCommentDefinitionTest >> testNewComment [
	| newComment |
	newComment := RGCommentDefinition new
		content: 'This is a comment for test';
		stamp: 'VeronicaUquillas 3/22/2011 14:51';
		yourself.

	self assert: newComment isComment.
	self assert: newComment isPassive.
	self assert: newComment name equals: #Comment.
	self assert: newComment hasStamp.
	self assert: newComment hasAuthor.
	self assert: newComment timeStamp notNil.

	self assert: newComment author equals: 'VeronicaUquillas'.
	self assert: newComment timeStamp equals: '3/22/2011 14:51' asDateAndTime.
	self assert: newComment parent equals: nil.
	self assert: newComment environment equals: Smalltalk globals
]

{ #category : #testing }
RGCommentDefinitionTest >> testSorting [

	| rgComm1 rgComm2 |
	rgComm1 := RGCommentDefinition realClass: RGClassDefinition.
	rgComm2 := RGCommentDefinition realClass: RGElementDefinition.
	self assert: rgComm1 <= rgComm2
]

{ #category : #testing }
RGCommentDefinitionTest >> testingConversion [
	
	| rgComm |
	rgComm := RGCommentDefinition realClass: RGClassDefinition.
	self assert: rgComm asString equals: 'RGClassDefinition Comment'.
	
	rgComm := RGCommentDefinition class: (RGClassDefinition named: #Foo).
	self assert: rgComm asString equals: 'Foo Comment'
]
