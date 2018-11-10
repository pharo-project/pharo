Class {
	#name : #TonelReaderTest,
	#superclass : #MCTestCase,
	#category : #'MonticelloTonel-Tests'
}

{ #category : #private }
TonelReaderTest class >> mockSnapshotSTON [
	^ 'MCSnapshot {
	#definitions : [
		MCOrganizationDefinition {
			#categories : [
				#MonticelloMocks
			]
		},
		MCClassDefinition {
			#name : #MCMockASubclass,
			#superclassName : #MCMockClassA,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''x''
				},
				MCClassVariableDefinition {
					#name : ''Y''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\r\r\tInitializationOrder := InitializationOrder\r\t\tifNil: [ -100 ] "let the test fail"\r\t\tifNotNil: [ InitializationOrder + 1.]'',
			#category : #''as yet unclassified'',
			#selector : #initialize,
			#className : #MCMockASubclass
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''variables\r\t^ x + Y + MCMockClassA'',
			#category : #''as yet unclassified'',
			#selector : #variables,
			#className : #MCMockASubclass
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''variables2\r\t^ ivar + CVar'',
			#category : #''as yet unclassified'',
			#selector : #variables2,
			#className : #MCMockASubclass
		},
		MCClassDefinition {
			#name : #MCMockClassA,
			#superclassName : #MCMock,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''ivar''
				},
				MCClassVariableDefinition {
					#name : ''CVar''
				},
				MCClassVariableDefinition {
					#name : ''InitializationOrder''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : ''This is a mock class. The Monticello tests manipulated it to simulate a developer modifying code in the image.'',
			#commentStamp : ''cwp 8/10/2003 16:43'',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''cVar\r\t^ CVar'',
			#category : #''as yet unclassified'',
			#selector : #cVar,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initializationOrder\r\r\t^ InitializationOrder'',
			#category : #''as yet unclassified'',
			#selector : #initializationOrder,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''initialize\r\tCVar := #initialized.\r\tInitializationOrder := 1.\r'',
			#category : #''as yet unclassified'',
			#selector : #initialize,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''one\r\r\t^ 1'',
			#category : #''as yet unclassified'',
			#selector : #one,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''touchCVar\r\tCVar := #touched'',
			#category : #''as yet unclassified'',
			#selector : #touchCVar,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''a\r\t^ \''a2\'''',
			#category : #numeric,
			#selector : #a,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''b\r\t^ \''b1\'''',
			#category : #numeric,
			#selector : #b,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''c\r\t^ \''c1\'''',
			#category : #numeric,
			#selector : #c,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''d\r\t^ \''d\'''',
			#category : #numeric,
			#selector : #d,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''falsehood\r\t^ false'',
			#category : #boolean,
			#selector : #falsehood,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''moreTruth\r\r\t^ true'',
			#category : #boolean,
			#selector : #moreTruth,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''one\r\t^ 1'',
			#category : #numeric,
			#selector : #one,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''q'',
			#category : #''drag\''n\''drop'',
			#selector : #q,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''truth\r\t^ true'',
			#category : #boolean,
			#selector : #truth,
			#className : #MCMockClassA
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''two\r\t^ 2'',
			#category : #numeric,
			#selector : #two,
			#className : #MCMockClassA
		},
		MCClassDefinition {
			#name : #MCMockClassB,
			#superclassName : #MCMock,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''ivarb''
				},
				MCClassVariableDefinition {
					#name : ''CVar''
				},
				MCPoolImportDefinition {
					#name : ''MCMockAPoolDictionary''
				},
				MCClassInstanceVariableDefinition {
					#name : ''ciVar''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : ''This comment has a bang! Bang! Bang!'',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassD,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''one\r\t^ 1'',
			#category : #''as yet unclassified'',
			#selector : #one,
			#className : #MCMockClassD
		},
		MCClassDefinition {
			#name : #MCMockClassE,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #variable,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''two\r\t^ 2'',
			#category : #''as yet unclassified'',
			#selector : #two,
			#className : #MCMockClassE
		},
		MCClassDefinition {
			#name : #MCMockClassF,
			#superclassName : #Object,
			#variables : OrderedCollection [
				MCClassVariableDefinition {
					#name : ''Foo''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassG,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #words,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassH,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #bytes,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCClassDefinition {
			#name : #MCMockClassI,
			#superclassName : #Object,
			#variables : OrderedCollection [ ],
			#category : #MonticelloMocks,
			#type : #weak,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : false,
			#source : ''mockClassExtension\r\r\t"I change the protocol of this method to resolve the failing test: MCChangeNotificationTest >> testExtMethodModified. This test basically test that when we modified an extension method, the extended package is marked as \''modified\''. The problem is that Monticello treat differently a classic method from an extension method, and this only by checking if the protocol name start with a star. Therefore, if the protocol does not match the extending package name, the extending package name will never be notified, and the test will fail. " '',
			#category : #''*MonticelloMocks'',
			#selector : #mockClassExtension,
			#className : #MCSnapshotTest
		}
	]
}'
]

{ #category : #private }
TonelReaderTest class >> mockSnapshotSTONWithCommentsInSelector [
	^ 'MCSnapshot {
	#definitions : [
		MCOrganizationDefinition {
			#categories : [
				#MonticelloMocks
			]
		},
		MCClassDefinition {
			#name : #MCMockASubclass,
			#superclassName : #MCMockClassA,
			#variables : OrderedCollection [
				MCInstanceVariableDefinition {
					#name : ''x''
				},
				MCClassVariableDefinition {
					#name : ''Y''
				}
			],
			#category : #MonticelloMocks,
			#type : #normal,
			#comment : '''',
			#commentStamp : '''',
			#traitComposition : ''{}'',
			#classTraitComposition : ''{}''
		},
		MCMethodDefinition {
			#classIsMeta : true,
			#source : ''block: aBlock "<BlockContext>" argsClass: argsProxyClass "<Alien class>" "^<Callback>"\r\t"Answer a Callback that evaluates aBlock and whose thunk should conform to the C calling convention."\r\t<legacy>\r\t^self new\r\t\tblock: aBlock\r\t\tthunk: FFICallbackThunk newCCall\r\t\targsProxyClass: argsProxyClass'',
			#category : #''as yet unclassified'',
			#selector : #''block:argsClass:'',
			#className : #MCMockASubclass
		}
	]
}'
]

{ #category : #tests }
TonelReaderTest >> assertClassDefinition: a and: b [
	self assert: a className equals: b className.
	self assert: a superclassName equals: b superclassName.
	self assert: a traitCompositionString equals: b traitCompositionString.
	self assert: a classTraitCompositionString equals: b classTraitCompositionString.
	self assert: a category equals: b category.	
	self assert: a instVarNames equals: b instVarNames.
	self assert: a classInstVarNames equals: b classInstVarNames.
	self assert: a classVarNames equals: b classVarNames.
	self assert: a poolDictionaries equals: b poolDictionaries.
	self assert: a type equals: b type.
	self assert: a comment equals: b comment.
]

{ #category : #tests }
TonelReaderTest >> assertDefinition: a and: b [
	a isOrganizationDefinition ifTrue: [ ^ self assertOrganisationDefinition: a and: b ].
	a isClassDefinition ifTrue: [ ^ self assertClassDefinition: a and: b ].
	a isMethodDefinition ifTrue: [ ^ self assertMethodDefinition: a and: b ].
]

{ #category : #tests }
TonelReaderTest >> assertMethodDefinition: a and: b [
	self assert: a className equals: b className.
	self assert: a selector equals: b selector.
	self assert: a protocol equals: b protocol.
	self assert: a source asByteArray equals: b source asByteArray.
	self assert: a classIsMeta equals: b classIsMeta
]

{ #category : #tests }
TonelReaderTest >> assertOrganisationDefinition: a and: b [
	self assert: a categories equals: b categories
]

{ #category : #mocks }
TonelReaderTest >> mockSnapshot [
	"HACK: since the serialization test changes the method definition (and it does not restores to 
	 previous value), validation fails randomly. And since what I will test does not needs that 
	 method, I will just remove it."
	^ STON fromString: self class mockSnapshotSTON
]

{ #category : #mocks }
TonelReaderTest >> mockSnapshotWithCommentsInSelector [
	"HACK: since the serialization test changes the method definition (and it does not restores to 
	 previous value), validation fails randomly. And since what I will test does not needs that 
	 method, I will just remove it."
	^ STON fromString: self class mockSnapshotSTONWithCommentsInSelector
]

{ #category : #'private factory' }
TonelReaderTest >> newMemoryFileSystemSnapshot: aSnapshop [
	"This method assumes writer works. If it is broken (the tests should show it), this tests 
	 will break too."
	| mem |
	mem := FileSystem memory root.
	(TonelWriter on: mem) writeSnapshot: aSnapshop.
	^ mem
]

{ #category : #tests }
TonelReaderTest >> testLoadDefinitions [
	| snapshot mem reader |
	
	snapshot := self mockSnapshot.
	mem := self newMemoryFileSystemSnapshot: snapshot.
	reader := TonelReader on: mem fileName: 'MonticelloMocks'.
	reader loadDefinitions.
	
	self 
		assert: reader definitions size 
		equals: snapshot definitions size.
		
	reader definitions sorted 
		with: snapshot definitions sorted
		do: [ :a :b | self assertDefinition: a and: b ]
]

{ #category : #tests }
TonelReaderTest >> testLoadSelectorWithComments [
	| snapshot mem reader |
	
	snapshot := self mockSnapshotWithCommentsInSelector.
	mem := self newMemoryFileSystemSnapshot: snapshot.
	reader := TonelReader on: mem fileName: 'MonticelloMocks'.
	reader loadDefinitions.
	
	self 
		assert: reader definitions size 
		equals: snapshot definitions size.
		
	reader definitions sorted 
		with: snapshot definitions sorted
		do: [ :a :b | self assertDefinition: a and: b ]
]
