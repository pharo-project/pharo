Class {
	#name : #TonelWriterTest,
	#superclass : #MCTestCase,
	#category : #'MonticelloTonel-Tests'
}

{ #category : #private }
TonelWriterTest >> expectedMCMockASubclassClassSt [
	^ 'Class {
	#name : #MCMockASubclass,
	#superclass : #MCMockClassA,
	#instVars : [
		''x''
	],
	#classVars : [
		''Y''
	],
	#category : #MonticelloMocks
}

{ #category : #''as yet unclassified'' }
MCMockASubclass class >> initialize [

	InitializationOrder := InitializationOrder
		ifNil: [ -100 ] "let the test fail"
		ifNotNil: [ InitializationOrder + 1.]
]

{ #category : #''as yet unclassified'' }
MCMockASubclass >> variables [
	^ x + Y + MCMockClassA
]

{ #category : #''as yet unclassified'' }
MCMockASubclass >> variables2 [
	^ ivar + CVar
]
'
]

{ #category : #private }
TonelWriterTest >> expectedMCMockClassAClassSt [
	^ '"
This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.
"
Class {
	#name : #MCMockClassA,
	#superclass : #MCMock,
	#instVars : [
		''ivar''
	],
	#classVars : [
		''CVar'',
		''InitializationOrder''
	],
	#category : #MonticelloMocks
}

{ #category : #''as yet unclassified'' }
MCMockClassA class >> cVar [
	^ CVar
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> initializationOrder [

	^ InitializationOrder
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> initialize [
	CVar := #initialized.
	InitializationOrder := 1.

]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> one [

	^ 1
]

{ #category : #''as yet unclassified'' }
MCMockClassA class >> touchCVar [
	CVar := #touched
]

{ #category : #numeric }
MCMockClassA >> a [
	^ ''a2''
]

{ #category : #numeric }
MCMockClassA >> b [
	^ ''b1''
]

{ #category : #numeric }
MCMockClassA >> c [
	^ ''c1''
]

{ #category : #numeric }
MCMockClassA >> d [
	^ ''d''
]

{ #category : #boolean }
MCMockClassA >> falsehood [
	^ false
]

{ #category : #boolean }
MCMockClassA >> moreTruth [

	^ true
]

{ #category : #numeric }
MCMockClassA >> one [
	^ 1
]

{ #category : #''drag\''n\''drop'' }
MCMockClassA >> q [
]

{ #category : #boolean }
MCMockClassA >> truth [
	^ true
]

{ #category : #numeric }
MCMockClassA >> two [
	^ 2
]
'
]

{ #category : #private }
TonelWriterTest >> expectedMCSnapshotTestExtensionSt [
	^ 'Extension { #name : #MCSnapshotTest }

{ #category : #''*MonticelloMocks'' }
MCSnapshotTest >> mockClassExtension [

	"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as ''modified''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. " 
]
'
]

{ #category : #tests }
TonelWriterTest >> testSplitMethodSourceInto [
	| writer declaration source definition newLine tab space |
	
	newLine := OSPlatform current lineEnding.
	tab := Character tab asString.
	space := Character space asString.
	writer := TonelWriter new.

	"simplest split"
	definition := MCMethodDefinition
		className: #SomeClass 
		selector: #name 
		category: 'accessing' 
		timeStamp: nil
		source: 'name
	^ self'.		
	writer splitMethodSource: definition into: [ :d :s | declaration := d. source := s ].
	
	self assert: declaration equals: 'name'.
	self assert: source equals: newLine, tab, '^ self'.
	
	"test space at the end of method declaration (it needs to be kept)"
	definition := MCMethodDefinition
		className: #SomeClass
		selector: #name:
		category: 'accessing'
		timeStamp: nil
		source: 'name: aString 
	name := aString'.
	writer splitMethodSource: definition into: [ :d :s | declaration := d. source := s ].
	self assert: declaration equals: 'name: aString'.
	self assert: source equals: space, newLine, tab, 'name := aString'.

	"test multiline declaration"
	definition := MCMethodDefinition
		className: #SomeClass
		selector: #method:with:several:lines:
		category: 'accessing'
		timeStamp: nil
		source: 'method: var1
	with: var2
	several: var3
	lines: var4
	
	^ var1 + var2 + var3 + var4'.
	writer splitMethodSource: definition into: [ :d :s | declaration := d. source := s ].
	self assert: declaration equals: ('method: var1
	with: var2
	several: var3
	lines: var4' withLineEndings: OSPlatform current lineEnding).
	self assert: source equals: (newLine, tab, newLine, tab, '^ var1 + var2 + var3 + var4' withLineEndings: newLine).

	"test comment before declaration (it may happen, if someone copied from diffmorph)"
	definition := MCMethodDefinition
		className: #SomeClass
		selector: #name:
		category: 'accessing'
		timeStamp: nil
		source: '
"protocol: accessing"
name: aString 
	name := aString'.
	writer splitMethodSource: definition into: [ :d :s | declaration := d. source := s ].
	self assert: declaration equals: 'name: aString'.
	self assert: source equals: space, newLine, tab, 'name := aString'.

	"test source right after declaration (no enter between selector and source)"
	definition := MCMethodDefinition
		className: #SomeClass
		selector: #name:
		category: 'accessing'
		timeStamp: nil
		source: 'name: aString name := aString'.
	writer splitMethodSource: definition into: [ :d :s | declaration := d. source := s ].
	self assert: declaration equals: 'name: aString'.
	self assert: source equals: ' name := aString'.
	
	"test method name containin name of keywords in variables"
	definition := MCMethodDefinition
		className: #SomeClass
		selector: #a:b:c:
		category: 'accessing'
		timeStamp: nil
		source: 'a: b b: c c: d ^ 42'.
	writer splitMethodSource: definition into: [ :d :s | declaration := d. source := s ].
	self assert: declaration equals: 'a: b b: c c: d'.
	self assert: source equals: ' ^ 42'.
]

{ #category : #tests }
TonelWriterTest >> testWriteClassDefinitionOn [
	| writer def stream |
	
	writer := TonelWriter new.

	stream := String new writeStream.
	def := MCClassDefinition
		name: #SomeObject
		superclassName: #Object
		traitComposition: '{}'
		classTraitComposition: '{}'
		category: #Kernel
		instVarNames: #()
		classVarNames: #()
		poolDictionaryNames: #()
		classInstVarNames: #()
		type: #normal
		comment: 'comment test'
		commentStamp: nil.
	writer writeClassDefinition: def on: stream. 
	self 
		assert: stream contents 
		equals: ('"
comment test
"
Class {
	#name : #SomeObject,
	#superclass : #Object,
	#category : #Kernel
}
' withLineEndings: OSPlatform current lineEnding).

	stream := String new writeStream.
	def := MCClassDefinition
		name: #SomeObject
		superclassName: #Object
		traitComposition: '{}'
		classTraitComposition: '{}'
		category: #Kernel
		instVarNames: #(a b c)
		classVarNames: #(D E)
		poolDictionaryNames: #(POOL)
		classInstVarNames: #(instVarA)
		type: #normal
		comment: 'comment test'
		commentStamp: nil.
	writer writeClassDefinition: def on: stream. 
	self 
		assert: stream contents 
		equals: ('"
comment test
"
Class {
	#name : #SomeObject,
	#superclass : #Object,
	#instVars : [
		''a'',
		''b'',
		''c''
	],
	#classVars : [
		''D'',
		''E''
	],
	#pools : [
		''POOL''
	],
	#classInstVars : [
		''instVarA''
	],
	#category : #Kernel
}
' withLineEndings: OSPlatform current lineEnding).

]

{ #category : #tests }
TonelWriterTest >> testWriteMethodDefinitionOn [
	| writer def stream |
	
	writer := TonelWriter new.

	stream := String new writeStream.
	def := MCMethodDefinition 
		className: #Object
		classIsMeta: false
		selector: #selector
		category: 'accessing' 
		timeStamp: nil
		source: 'selector
	^ 42'.
	writer writeMethodDefinition: def on: stream. 
	self 
		assert: stream contents 
		equals: ('
{ #category : #accessing }
Object >> selector [
	^ 42
]
' withLineEndings: OSPlatform current lineEnding).

	stream := String new writeStream.
	def := MCMethodDefinition 
		className: #Object
		classIsMeta: true
		selector: #selector
		category: 'accessing' 
		timeStamp: nil
		source: 'selector
	^ 42'.
	writer writeMethodDefinition: def on: stream. 
	self 
		assert: stream contents 
		equals: ('
{ #category : #accessing }
Object class >> selector [
	^ 42
]
' withLineEndings: OSPlatform current lineEnding).


	stream := String new writeStream.
	def := MCMethodDefinition 
		className: #Object
		classIsMeta: false
		selector: #=
		category: 'comparing' 
		timeStamp: nil
		source: '= anObject
	^ self == anObject'.
	writer writeMethodDefinition: def on: stream. 
	self 
		assert: stream contents 
		equals: ('
{ #category : #comparing }
Object >> = anObject [
	^ self == anObject
]
' withLineEndings: OSPlatform current lineEnding).
]

{ #category : #tests }
TonelWriterTest >> testWriteSnapshot [
	| writer mem nl |
	
	mem := FileSystem memory root.
	writer := TonelWriter on: mem.
	writer writeSnapshot: self mockSnapshot.
	
	self assert: (mem / 'MonticelloMocks') exists.
	self 
		assert: ((mem / 'MonticelloMocks') entries collect: #basename) 
		equals: #(
			'MCMockASubclass.class.st' 
			'MCMockClassA.class.st' 
			'MCMockClassB.class.st' 
			'MCMockClassD.class.st' 
			'MCMockClassE.class.st' 
			'MCMockClassF.class.st' 
			'MCMockClassG.class.st' 
			'MCMockClassH.class.st' 
			'MCMockClassI.class.st' 
			'MCSnapshotTest.extension.st'
			'package.st').
	
	nl := OSPlatform current lineEnding.
	self 
		assert: (mem / 'MonticelloMocks' / 'MCMockClassA.class.st') contents
		equals: (self expectedMCMockClassAClassSt withLineEndings: nl).
	self 
		assert: (mem / 'MonticelloMocks' / 'MCMockASubclass.class.st') contents
		equals: (self expectedMCMockASubclassClassSt withLineEndings: nl).
	self 
		assert: (mem / 'MonticelloMocks' / 'MCSnapshotTest.extension.st') contents
		equals: (self expectedMCSnapshotTestExtensionSt withLineEndings: nl)
]
