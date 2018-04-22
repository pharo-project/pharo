Class {
	#name : #MCStWriterTest,
	#superclass : #MCTestCase,
	#instVars : [
		'stream',
		'writer'
	],
	#category : #'Monticello-Tests-IO'
}

{ #category : #asserting }
MCStWriterTest >> assertAllChunksAreWellFormed [
	stream := ChunkReadStream on: stream contents readStream.
	self assertChunkIsWellFormed: stream next.
]

{ #category : #asserting }
MCStWriterTest >> assertChunkIsWellFormed: chunk [
	self class compiler
		source:  chunk readStream ;
		class: UndefinedObject;
		noPattern: true;
		failBlock:  [self assert: false];
		compile.
]

{ #category : #asserting }
MCStWriterTest >> assertContentsOf: strm match: expected [ 
	| actual |
	actual := strm contents.
	self assert: actual size = expected size.
	actual with: expected do: [:a :e | self assert: a = e]
]

{ #category : #asserting }
MCStWriterTest >> assertMethodChunkIsWellFormed: chunk [
	self class compiler
		source: chunk readStream;
		class: UndefinedObject;
		failBlock: [self assert: false];
		compile.
	
]

{ #category : #data }
MCStWriterTest >> expectedClassDefinitionA [
 ^ '
MCMock subclass: #MCMockClassA
	instanceVariableNames: ''ivar''
	classVariableNames: ''CVar InitializationOrder''
	poolDictionaries: ''''
	category: ''MonticelloMocks''!

!MCMockClassA commentStamp: '''' prior: 0!
This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.!
'
]

{ #category : #data }
MCStWriterTest >> expectedClassDefinitionB [
 ^ '
MCMock subclass: #MCMockClassB
	instanceVariableNames: ''ivarb''
	classVariableNames: ''CVar''
	poolDictionaries: ''MCMockAPoolDictionary''
	category: ''MonticelloMocks''!

MCMockClassB class
	instanceVariableNames: ''ciVar''!

!MCMockClassB commentStamp: '''' prior: 0!
This comment has a bang!! Bang!! Bang!!!
'
]

{ #category : #data }
MCStWriterTest >> expectedClassMethodDefinition [

	^ '\s*!MCMockClassA class methodsFor\: ''as yet unclassified'' stamp\: ''[/:\s\w]*''!\s*one\s*\^ 1! !\s*'.
]

{ #category : #testing }
MCStWriterTest >> expectedInitializerA [
	^ 'MCMockClassA initialize'
]

{ #category : #testing }
MCStWriterTest >> expectedInitializerASubclass [
	^ 'MCMockASubclass initialize'
]

{ #category : #data }
MCStWriterTest >> expectedMethodDefinition [
	^ '\s*!MCMockClassA methodsFor\: ''numeric'' stamp\: ''[/:\s\w]*''!\s*one\s*\^ 1! !\s*'
]

{ #category : #data }
MCStWriterTest >> expectedMethodDefinitionWithBangs [
	^ '
!MCStWriterTest methodsFor: ''testing'' stamp: ''''!
methodWithBangs
	^ ''
	^ ReadStream on: 
''''MCRevisionInfo packageName: ''''MonticelloCompatibilityTest''''!!!!
MCOrganizationDeclaration categories: 
  #(
  ''''MonticelloMocks'''')!!!!

MCClassDeclaration
  name: #MCMockClassD
  superclassName: #Object
  category: #''''MonticelloMocks''''
  instVarNames: #()
  comment: ''''''''!!!!

MCMethodDeclaration className: #MCMockClassD selector: #one category: #''''as yet unclassified'''' timeStamp: ''''cwp 7/8/2003 21:21'''' source: 
''''one
	^ 1''''!!!!
''''
''
! !
'
]

{ #category : #data }
MCStWriterTest >> expectedOrganizationDefinition [
	^ 'SystemOrganization addCategory: #MonticelloMocks!
'
]

{ #category : #testing }
MCStWriterTest >> methodWithBangs [
	^ '
	^ ReadStream on: 
''MCRevisionInfo packageName: ''MonticelloCompatibilityTest''!!
MCOrganizationDeclaration categories: 
  #(
  ''MonticelloMocks'')!!

MCClassDeclaration
  name: #MCMockClassD
  superclassName: #Object
  category: #''MonticelloMocks''
  instVarNames: #()
  comment: ''''!!

MCMethodDeclaration className: #MCMockClassD selector: #one category: #''as yet unclassified'' timeStamp: ''cwp 7/8/2003 21:21'' source: 
''one
	^ 1''!!
''
'

]

{ #category : #running }
MCStWriterTest >> setUp [
	super setUp.
	stream := String new writeStream.
	writer := MCStWriter on: stream.

]

{ #category : #testing }
MCStWriterTest >> testClassDefinitionA [
	writer visitClassDefinition: (self mockClassA asClassDefinition).
	self assertContentsOf: stream match: self expectedClassDefinitionA.
	stream := ChunkReadStream on: stream contents readStream.
	2 timesRepeat: [self assertChunkIsWellFormed: stream next]
]

{ #category : #testing }
MCStWriterTest >> testClassDefinitionB [
	writer visitClassDefinition: (self mockClassB asClassDefinition).
	self assertContentsOf: stream match: self expectedClassDefinitionB.
	
]

{ #category : #testing }
MCStWriterTest >> testClassMethodDefinition [
	writer visitMethodDefinition: (RGMethodDefinition realClass: self mockClassA class selector: #one) asMCMethodDefinition.
	self assert: (stream contents matchesRegex: self expectedClassMethodDefinition).
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 
]

{ #category : #testing }
MCStWriterTest >> testInitializerDefinition [
	|chunk lastChunk penultChunk|
	writer writeSnapshot: self mockSnapshot.
	stream := ChunkReadStream on: stream contents readStream.
	
	[stream atEnd] whileFalse:
		[chunk := stream next.
		chunk isAllSeparators ifFalse: [
			penultChunk := lastChunk.
			lastChunk := chunk]].
	self assertContentsOf: penultChunk readStream match: self expectedInitializerA.
	self assertContentsOf: lastChunk readStream match: self expectedInitializerASubclass
]

{ #category : #testing }
MCStWriterTest >> testMethodDefinition [
	writer visitMethodDefinition: (RGMethodDefinition realClass: self mockClassA selector: #one) asMCMethodDefinition.
	self assert: (stream contents matchesRegex: self expectedMethodDefinition).
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 
]

{ #category : #testing }
MCStWriterTest >> testMethodDefinitionWithBangs [

	| methodDefinition |
	
	methodDefinition := (RGMethodDefinition 
		realClass: self class
		selector: #methodWithBangs) asMCMethodDefinition.	
	methodDefinition setTimeStamp: ''.
	writer visitMethodDefinition: methodDefinition.
										
	self assertContentsOf: stream contents match: self expectedMethodDefinitionWithBangs.
	
	stream := ChunkReadStream on: stream contents readStream.
	
	self assert: stream next isAllSeparators.
	self assertChunkIsWellFormed: stream next.
	self assertMethodChunkIsWellFormed: stream next.
	self assert: stream next isAllSeparators 
]

{ #category : #testing }
MCStWriterTest >> testNotLoadedClassMethod [
	"Check that the chunk header is correct for a class method of a class that isn't part of the running system"
	| definition methodStream methodWriter |

	definition := MCMethodDefinition
		className: #NotALoadedClass
		classIsMeta: true
		selector: #reset
		category: 'imagine'
		timeStamp: nil
		source: 'reset

    ^self doReset'.
	methodStream := String new writeStream.
	methodWriter := MCStWriter new
		stream: methodStream;
		yourself.

	methodWriter writeMethodPreamble: definition.
	self assert: methodStream contents trimBoth
			equals: '!NotALoadedClass class methodsFor: ''imagine'' stamp: ''nil''!'.

]

{ #category : #testing }
MCStWriterTest >> testOrganizationDefinition [
	| definition |
	definition := MCOrganizationDefinition categories: 
					(self mockPackage packageSet systemCategories).
	writer visitOrganizationDefinition: definition.
	self assertContentsOf: stream match: self expectedOrganizationDefinition.
	self assertAllChunksAreWellFormed.
]
