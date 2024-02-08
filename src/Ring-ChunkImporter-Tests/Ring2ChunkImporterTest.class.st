"
I'm a nice comment!, treat me well :).
"
Class {
	#name : 'Ring2ChunkImporterTest',
	#superclass : 'RGTest',
	#category : 'Ring-ChunkImporter-Tests',
	#package : 'Ring-ChunkImporter-Tests'
}

{ #category : 'tests' }
Ring2ChunkImporterTest >> testClassWithNilSuperclass [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'SomeClass subclass: #SomeClass
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''SomePackage''.
SomeClass superclass: nil!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNotNil.
	self assert: class instVarNames isEmpty.
	self assert: class classVarNames isEmpty.
	self assert: class package name equals: 'SomePackage'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testClassWithSimpleSlots [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SlotExampleMovie
	slots: { #name. #year. #director. #actors }
	classVariables: {  }
	category: ''Slot-Tests-Associations''!' readStream.

	class := importer environment ask behaviorNamed: #SlotExampleMovie.
	self assert: class isNotNil.
	self assert: class instVarNames asArray equals: #(#name #year #director #actors).
	self assert: class classVarNames isEmpty.
	self assert: class category equals: 'Slot-Tests-Associations'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testClassWithUnknownSlots [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SlotExampleMovie
	slots: { #name. #year. #director => ToOneRelationSlot.
				#actors => ToManyRelationSlot }
	classVariables: {  }
	category: ''Slot-Tests-Associations''!' readStream.

	class := importer environment ask behaviorNamed: #SlotExampleMovie.
	self assert: class isNotNil.
	self assert: class instVarNames asArray equals: #(#name #year #director #actors).
	self assert: class classVarNames isEmpty.
	self assert: class category equals: 'Slot-Tests-Associations'.
	self assert: ((class slots last: 2) allSatisfy: [ :each | each isKindOf: RGUnknownSlot  ])
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testClassWithUnknownSlotsWithMessages [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SlotExampleMovie
	slots: { #name. #year. #director => ToOneRelationSlot inverse: #directedMovies inClass: #SlotExamplePerson.
				#actors => ToManyRelationSlot inverse: #actedInMovies inClass: #SlotExamplePerson }
	classVariables: {  }
	category: ''Slot-Tests-Associations''!' readStream.

	class := importer environment ask behaviorNamed: #SlotExampleMovie.
	self assert: class isNotNil.
	self assert: class instVarNames asArray equals: #(#name #year #director #actors).
	self assert: class classVarNames isEmpty.
	self assert: class category equals: 'Slot-Tests-Associations'.
	self assert: ((class slots last: 2) allSatisfy: [ :each | each isKindOf: RGUnknownSlot  ])
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testHistoricalStandardComment [

	| importer class comment code |

	importer := RGChunkImporter new.

	code := self testingHistoricalCommentFor: #SomeClass.

	importer fileInFrom: code readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	comment := class comment.

	self assert: comment isNotNil.
	self assert: comment isComment.
	self assert: comment isRingResolved.
	self assert: (comment hasResolvedAll: #(content author time)).

	self assert: comment content equals: 'I''m a nice comment!, treat me well :).'.
	self assert: comment author equals: '<historical>'.
	self assert: comment time equals: DateAndTime new
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testMetaclassWithTrait [

	| importer class |

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass class
	uses: SomeTrait classTrait
	instanceVariableNames: ''''' readStream.
	class := importer environment ask behaviorNamed: 'SomeClass class'.
	self assert: class isNotNil.
	self assert: class isMetaclass.
	self assert: class instVarNames isEmpty.
	self assert: (importer environment ask behaviorNamed: 'SomeClass') isNotNil.
	self assert: (importer environment ask behaviorNamed: 'SomeTrait classTrait') isNotNil.
	self assert: (importer environment ask behaviorNamed: 'SomeTrait') isNotNil.
	self assert: class traitComposition transformations size equals: 1.
	self assert: class traitComposition transformations first name equals: 'SomeTrait classTrait'.
	self assert: class traitComposition transformations first isTrait
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testMultipleChunks [
	| importer class |
	importer := RGChunkImporter new.

	importer
		fileInFrom:
			'Object subclass: #SomeClass!
Object subclass: #SomeClass2!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNotNil.
	self assert: class superclass name equals: #Object.

	class := importer environment ask behaviorNamed: #SomeClass2.
	self assert: class isNotNil.
	self assert: class superclass name equals: #Object
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testMultipleClassChunksInStrangeOrder [
	| importer class class2 |
	importer := RGChunkImporter new.

	importer
		fileInFrom:
			'SomeClass2 subclass: #SomeClass!
Object subclass: #SomeClass2!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNotNil.
	self assert: class superclass name equals: #SomeClass2.

	class2 := importer environment ask behaviorNamed: #SomeClass2.
	self assert: class2 isNotNil.
	self assert: class2 superclass name equals: #Object.

	self assert: class superclass identicalTo: class2
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testMultipleMixedChunksInStrangeOrder [
	| importer class method |
	importer := RGChunkImporter new.

	importer
		fileInFrom:
			'!SomeClass methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!
someMethod
	^true! !
Object subclass: #SomeClass!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNotNil.
	self assert: class superclass name equals: #Object.
	method := class methodNamed: #someMethod.
	self assert: method isNotNil.
	self assert: (method sourceCode endsWith: '^true')
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testMultipleMixedChunksInStrangeOrderWithMetaclassTrait [

	| importer trait method |

	<expectedFailure>
	"This test currently needs to fail because the CodeImporter gives only original behavior name and #isMata.
	We do not know that the behavior was supposed to be classTrait."

	importer := RGChunkImporter new.

	importer fileInFrom: '!TEmpty classTrait methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!
someMethod
	^true! !
Trait named: #TEmpty
	uses: {}
	package: ''Traits-Kernel-Traits''!' readStream.

	trait := importer environment ask behaviorNamed: #'TEmpty classTrait'.
	self assert: trait isNotNil.
	self assert: (trait isTrait and: [ trait isMetaclassTrait not ]).
	method := trait methodNamed: #someMethod.
	self assert: method isNotNil.
	self assert: (method sourceCode endsWith: '^true')
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testMultipleMixedChunksInStrangeOrderWithTrait [

	| importer trait method |

	importer := RGChunkImporter new.

	importer fileInFrom: '!TEmpty methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!
someMethod
	^true! !
Trait named: #TEmpty
	uses: {}
	package: ''Traits-Kernel-Traits''!' readStream.

	trait := importer environment ask behaviorNamed: #TEmpty.
	self assert: trait isNotNil.
	self assert: (trait isTrait and: [ trait isMetaclassTrait not ]).
	method := trait methodNamed: #someMethod.
	self assert: method isNotNil.
	self assert: (method sourceCode endsWith: '^true')
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testProtoObject [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'ProtoObject subclass: #ProtoObject
	instanceVariableNames: ''''
	classVariableNames: ''''
	package: ''Kernel-Objects''.
ProtoObject superclass: nil!' readStream.

	class := importer environment ask behaviorNamed: #ProtoObject.
	self assert: class isNotNil.
	self assert: class superclass isNil.
	self assert: class instVarNames isEmpty.
	self assert: class classVarNames isEmpty.
	self assert: class package name equals: 'Kernel-Objects'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testSpecialSuperclasses [

	| importer class |

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass subclass: #SomeClass' readStream.
	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNotNil.
	self assert: class superclass equals: class.

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass subclass: #SomeClass.
	SomeClass superclass: nil.!' readStream.
	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNotNil.
	self assert: class superclass isNil.

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass subclass: #SomeClass.
	SomeOtherClass superclass: nil.!' readStream.
	" Only a DoIt, because  we specify different superclass. "
	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNil.
	importer doIts notEmpty.

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass subclass: #SomeClass.
	SomeClass superclass: self.!' readStream.
	" Only a DoIt, because  we do not specify superclass as nil. "
	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNil.
	importer doIts notEmpty.

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass subclass: #SomeClass.
	SomeClass yourself.
	SomeClass superclass: nil.!' readStream.
	" Only a DoIt, because we do not keep expected format "
	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class isNil.
	importer doIts notEmpty
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardClass [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			category: ''SomePackage''!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class instVarNames isEmpty.
	self assert: (class hasResolved: #layout).
	self assert: (class layout hasResolved: #slots).
	self assert: (class hasResolved: #classVariables).
	self assert: (class classVariables isEmpty).
	self assert: (class hasResolved: #sharedPools).
	self assert: (class sharedPoolNames isEmpty).
	self assert: class category equals: #SomePackage
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardClassWithPackage [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SomeClass
			instanceVariableNames: ''''
			classVariableNames: ''''
			poolDictionaries: ''''
			package: ''SomePackage''!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class instVarNames isEmpty.
	self assert: (class hasResolved: #layout).
	self assert: (class layout hasResolved: #slots).
	self assert: (class hasResolved: #classVariables).
	self assert: (class classVariables isEmpty).
	self assert: (class hasResolved: #sharedPools).
	self assert: (class sharedPoolNames isEmpty).
	self assert: class package name equals: #SomePackage
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardClassWithVariables [


	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SomeClass
			instanceVariableNames: ''var1 var2 var3''
			classVariableNames: ''ClassVar1 ClassVar2 ClassVar3''
			poolDictionaries: ''Pool1 Pool2 Pool3''
			category: ''SomePackage''!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.

	self assert: (class hasResolved: #layout).
	self assert: class instVarNames equals: #(var1 var2 var3).
	self assert: (class instVarNames allSatisfy: #isSymbol).

	self assert: (class hasResolved: #classVariables).
	self assert: class classVarNames equals: #(ClassVar1 ClassVar2 ClassVar3).
	self assert: (class instVarNames allSatisfy: #isSymbol).

	self assert: (class hasResolved: #sharedPools).
	self assert: class sharedPoolNames equals: #(Pool1 Pool2 Pool3).
	self assert: (class sharedPoolNames allSatisfy: #isSymbol).

	self assert: class category equals: #SomePackage
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardComment [

	| importer class comment code |

	importer := RGChunkImporter new.

	code := self testingCommentFor: #SomeClass.

	importer fileInFrom: code readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	comment := class comment.

	self assert: comment isNotNil.
	self assert: comment isComment.
	self assert: comment isRingResolved.
	self assert: (comment hasResolvedAll: #(content author time)).

	self assert: comment content equals: 'I''m a nice comment!, treat me well :).'.
	self assert: comment author equals: 'GuillermoPolito'.
	self assert: comment time equals: ('5/2/2012 13:35' asDateAndTime)
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMetaclass [

	| importer class |

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass class
	instanceVariableNames: ''''' readStream.
	class := importer environment ask behaviorNamed: 'SomeClass class'.
	self assert: class isNotNil.
	self assert: class isMetaclass.
	self assert: class instVarNames isEmpty.
	self assert: (importer environment ask behaviorNamed: 'SomeClass') isNotNil.

	importer := RGChunkImporter new.
	importer fileInFrom: 'SomeClass class
	instanceVariableNames: ''var1 var2 var3''' readStream.
	class := importer environment ask behaviorNamed: 'SomeClass class'.
	self assert: class isNotNil.
	self assert: class isMetaclass.
	self assert: class instVarNames equals: #(var1 var2 var3)
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMetaclassMethod [

	| importer class method code |

	importer := RGChunkImporter new.

	code := '!SomeClass class methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	class := importer environment ask behaviorNamed: #'SomeClass class'.
	method := class methodNamed: #someMethod.

	self assert: class isMetaclass.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name tags sourceCode author time)).

	self assert: method name equals: #someMethod.
	self assert: method protocol equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMetaclassTrait [

	| importer trait |

	importer := RGChunkImporter new.

	importer fileInFrom: 'TEmpty classTrait
	uses: {}' readStream.

	trait := importer environment ask behaviorNamed: #'TEmpty classTrait'.
	self assert: trait isNotNil.
	self assert: trait isMetaclassTrait.
	self assert: trait traitComposition transformations isEmpty
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMetaclassTraitWithMultipleTraits [

	| importer trait usedTraits |
	importer := RGChunkImporter new.
	importer fileInFrom: 'TEmpty classTrait
	uses: TIsEmpty classTrait + TComparable classTrait + TTransformationCompatibility classTrait' readStream.
	trait := importer environment ask behaviorNamed: #'TEmpty classTrait'.
	self assert: trait isNotNil.
	self assert: trait isMetaclassTrait.
	self assert: trait traitComposition transformations notEmpty.
	usedTraits := trait traitComposition transformations.
	self assert: (usedTraits allSatisfy: [:each | each isTrait]).
	self assert: (usedTraits collect: [:each | each name]) asArray equals: #('TIsEmpty classTrait' 'TComparable classTrait' 'TTransformationCompatibility classTrait')
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMetaclassTraitWithSingleTrait [

	| importer trait usedTrait |
	importer := RGChunkImporter new.

	importer fileInFrom: 'TEmpty classTrait
	uses: TIsEmpty classTrait' readStream.

	trait := importer environment ask behaviorNamed: #'TEmpty classTrait'.
	self assert: trait isNotNil.
	self assert: trait isMetaclassTrait.
	self assert: trait traitComposition transformations notEmpty.
	usedTrait := trait traitComposition transformations first.
	self assert: usedTrait name equals: #'TIsEmpty classTrait'.
	self assert: (importer environment ask behaviorNamed: #'TIsEmpty classTrait') isNotNil
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMethod [

	| importer class method code |

	importer := RGChunkImporter new.

	code := '!SomeClass methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	method := class methodNamed: #someMethod.

	self assert: class isClass.
	self assert: method isNotNil.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name sourceCode author time tags)).

	self assert: method name equals: #someMethod.
	self assert: method protocol equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMethodInExistingClass [

	| importer class method code |

	importer := RGChunkImporter new.
	class := importer environment ensureClassNamed: #SomeClass.

	code := '!SomeClass methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	method := class methodNamed: #someMethod.

	self assert: class isClass.
	self assert: method isNotNil.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name sourceCode author time tags)).

	self assert: method name equals: #someMethod.
	self assert: method protocol equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMethodInExistingMetaclassTrait [

	| importer trait method code |

	importer := RGChunkImporter new.

	trait := importer environment ensureMetaclassTraitNamed: #'SomeTrait classTrait'.

	code := '!SomeTrait classTrait methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	method := trait methodNamed: #someMethod.

	self assert: (trait isMetaclassTrait).
	self assert: method isNotNil.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name sourceCode author time tags)).

	self assert: method name equals: #someMethod.
	self assert: method protocol equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMethodInExistingTrait [

	| importer trait method code |

	importer := RGChunkImporter new.

	trait := importer environment ensureTraitNamed: #'SomeTrait'.

	code := '!SomeTrait methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	method := trait methodNamed: #someMethod.

	self assert: (trait isTrait and: [trait isMetaclassTrait not]).
	self assert: method isNotNil.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name sourceCode author time tags)).

	self assert: method name equals: #someMethod.
	self assert: method protocol equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMethodInNewMetaclass [

	| importer class method code |

	importer := RGChunkImporter new.

	code := '!SomeClass class methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	method := class metaclass methodNamed: #someMethod.

	self assert: class isClass.
	self assert: method isNotNil.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name tags sourceCode author time)).

	self assert: method name equals: #someMethod.
	self assert: method protocol equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardMethodInNewMetaclassTrait [

	| importer trait method code |

	<expectedFailure>
	"This test currently needs to fail because the CodeImporter gives only original behavior name and #isMata.
	We do not know that the behavior was supposed to be classTrait."

	importer := RGChunkImporter new.

	code := '!SomeTrait classTrait methodsFor: ''some protocol'' stamp: ''GuillermoPolito 5/2/2012 13:35''!someMethod
	^true'.

	importer fileInFrom: code readStream.

	trait := importer environment ask behaviorNamed: #SomeTrait.
	method := trait metaclass methodNamed: #someMethod.

	self assert: trait isTrait.
	self assert: trait classTrait isMetaclassTrait.
	self assert: method isNotNil.
	self assert: method isRingResolved.
	self assert: (method hasResolvedAll: #(name protocol sourceCode author time)).

	self assert: method name equals: #someMethod.
	self assert: method protocol name equals: 'some protocol'.
	self assert: method author equals: 'GuillermoPolito'.
	self assert: method time equals: ('5/2/2012 13:35' asDateAndTime).
	self assert: method sourceCode equals: 'someMethod
	^true'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardSubclasses [

	| importer class |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Object subclass: #SomeClass!' readStream.

	class := importer environment ask behaviorNamed: #SomeClass.
	self assert: class superclass name equals: #Object
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardTrait [

	| importer trait |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Trait named: #TIsEmpty
	uses: {}
	package: ''Traits-Kernel-Traits''' readStream.

	trait := importer environment ask behaviorNamed: #TIsEmpty.
	self assert: trait isNotNil.
	self assert: (trait isTrait and: [trait isMetaclassTrait not]).
	self assert: trait traitComposition transformations isEmpty.
	self assert: trait package name equals: 'Traits-Kernel-Traits'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardTraitWithExclusions [

	| importer trait usedTraits |
	importer := RGChunkImporter new.

	importer fileInFrom: 'Trait named: #TEmpty
	uses: TComparable - {#<=. #>}
	category: ''Traits-Kernel''' readStream.

	trait := importer environment ask behaviorNamed: #TEmpty.
	self assert: trait isNotNil.
	self assert: trait traitComposition transformations notEmpty.
	usedTraits := trait traitComposition transformations.
	self assert: usedTraits size equals: 1.
	self assert: usedTraits first subject name equals: 'TComparable'.
	self assert: usedTraits first removedSelectors asArray equals: #( #'<=' #> ).

	self assert: trait category equals: 'Traits-Kernel'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardTraitWithMultipleTraits [

	| importer trait usedTraits |
	importer := RGChunkImporter new.
	importer fileInFrom: 'Trait named: #TEmpty
	uses: TComparable + TIsEmpty + TTransformationCompatibility
	category: ''Traits-Kernel''' readStream.

	trait := importer environment ask behaviorNamed: #TEmpty.
	self assert: trait isNotNil.
	self assert: trait traitComposition transformations notEmpty.
	usedTraits := trait traitComposition transformations.
	self assert: (usedTraits allSatisfy: [:each | each isTrait]).
	self assert: (usedTraits collect: [:each | each name]) asArray equals: #(TComparable TIsEmpty TTransformationCompatibility).
	self assert: trait category equals: 'Traits-Kernel'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardTraitWithSingleTrait [
	| importer trait usedTrait |
	importer := RGChunkImporter new.

	importer
		fileInFrom:
			'Trait named: #TEmpty
	uses: TIsEmpty
	package: ''Traits-Kernel-Traits''' readStream.

	trait := importer environment ask behaviorNamed: #TEmpty.
	self assert: trait isNotNil.
	self assert: trait traitComposition transformations notEmpty.
	usedTrait := trait traitComposition transformations first.
	self assert: usedTrait name equals: #TIsEmpty.
	self assert: (importer environment ask behaviorNamed: #TIsEmpty) isNotNil.
	self assert: trait category equals: 'Traits-Kernel-Traits'
]

{ #category : 'tests' }
Ring2ChunkImporterTest >> testStandardTraitWithTraits [

	| importer trait |

	importer := RGChunkImporter new.

	importer fileInFrom: 'Trait named: #TEmpty
	uses: {}
	package: ''Traits-Kernel-Traits''' readStream.

	trait := importer environment ask behaviorNamed: #TEmpty.
	self assert: trait isNotNil.
	self assert: trait traitComposition transformations isEmpty.
	self assert: trait package name equals: 'Traits-Kernel-Traits'
]

{ #category : 'private - utilities' }
Ring2ChunkImporterTest >> testingCommentFor: aClassName [

	| comment commentToWrite|

	comment := 'I''m a nice comment!, treat me well :).'.
	commentToWrite := 'I''m a nice comment!!, treat me well :).' replaceAll: '!' with: '!!'.
	^ ('!{1} commentStamp: ''GuillermoPolito 5/2/2012 13:35'' prior: 0!{2}!' format: { aClassName. commentToWrite })
]

{ #category : 'private - utilities' }
Ring2ChunkImporterTest >> testingHistoricalCommentFor: aClassName [

	| comment commentToWrite|

	comment := 'I''m a nice comment!, treat me well :).'.
	commentToWrite := 'I''m a nice comment!!, treat me well :).' replaceAll: '!' with: '!!'.
	^ ('!{1} commentStamp: ''<historical>'' prior: 0!{2}!' format: { aClassName. commentToWrite })
]
