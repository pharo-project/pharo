"
I know how to map an object to its default cluster. I can map every object.
"
Class {
	#name : 'FLFullGeneralMapper',
	#superclass : 'FLLightGeneralMapper',
	#instVars : [
		'internalMethods',
		'internalBehaviors',
		'environment',
		'globals'
	],
	#category : 'Fuel-Core-Mappers',
	#package : 'Fuel-Core',
	#tag : 'Mappers'
}

{ #category : 'accessing' }
FLFullGeneralMapper >> environment [
	^ environment
]

{ #category : 'initialization' }
FLFullGeneralMapper >> initialize [
	super initialize.
	
	internalBehaviors := self configuration behaviorsToSerialize.
	internalMethods := self configuration methodsToSerialize.
	globals := self configuration globals.
	environment := self configuration environment
]

{ #category : 'testing' }
FLFullGeneralMapper >> isBehaviorAnonymous: aBehavior [
	^ (self environment includes: aBehavior instanceSide)
		ifTrue: [ false ]
		ifFalse: [
			aBehavior isObsolete ifTrue: [
				FLObsolete signalWithName: aBehavior name ].
			 true ]
]

{ #category : 'testing' }
FLFullGeneralMapper >> shouldSerializeBehaviorFully: aBehavior [
	^ (internalBehaviors includes: aBehavior instanceSide) or: [
		(internalBehaviors includes: aBehavior classSide) or: [
			self isBehaviorAnonymous: aBehavior ] ]
]

{ #category : 'testing' }
FLFullGeneralMapper >> shouldSerializeCompiledMethodFully: aCompiledMethod [
	"Answers true when aCompiledMethod must be fully serialized, and false otherwise.
	Methods that can be looked up in the environment must not be fully serialized.
	The following methods must be fully serialized:
		- methods that are included in the classes considered as internal
		- methods considered internal
		- methods that are DoIts"

	^ (self shouldSerializeBehaviorFully: aCompiledMethod methodClass) or: [
		(internalMethods includes: aCompiledMethod) or: [
			aCompiledMethod isInstalled not or: [
				aCompiledMethod isDoIt ] ] ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitClass: aClass [
	(self shouldSerializeBehaviorFully: aClass)
		ifTrue: [
			self
				mapAndTraceByClusterName: aClass
				to: FLClassCluster ]
		ifFalse: [ next mapAndTrace: aClass ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitClassVariable: aClassVariable [
	"Consider class variables regular objects (not globals), unless
	their owning class is being fully serialized, in which case the class
	variable needs to be serialized fully as well."
	(self shouldSerializeBehaviorFully: aClassVariable owningClass)
		ifFalse: [
			self
				mapAndTraceByClusterName: aClassVariable
				to: FLGlobalClassVariableCluster ]
		ifTrue: [ next mapAndTrace: aClassVariable ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitCompiledBlock: aCompiledBlock [
	(self shouldSerializeCompiledMethodFully: aCompiledBlock method)
		ifTrue: [
			self
				mapAndTraceByClusterName: aCompiledBlock
				to: FLCompiledBlockCluster ]
		ifFalse: [ next mapAndTrace: aCompiledBlock ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitCompiledMethod: aCompiledMethod [
	(self shouldSerializeCompiledMethodFully: aCompiledMethod)
		ifTrue: [
			self
				mapAndTraceByClusterName: aCompiledMethod
				to: FLCompiledMethodCluster ]
		ifFalse: [ next mapAndTrace: aCompiledMethod ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitGlobalVariable: aGlobalVariable [
	"Consider global variables globals that can be looked up, unless
	the associated class (if any) should be fully serialized."
	((globals includesKey: aGlobalVariable key) or: [
		(aGlobalVariable value isBehavior and: [
			self shouldSerializeBehaviorFully: aGlobalVariable value ]) not ])
				ifTrue: [
					self
						mapAndTraceByClusterName: aGlobalVariable
						to: FLGlobalAssociationCluster ]
				ifFalse: [ next mapAndTrace: aGlobalVariable ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitMetaclass: aMetaclass [
	(self shouldSerializeBehaviorFully: aMetaclass)
		ifTrue: [
			self
				mapAndTraceByClusterName: aMetaclass
				to: FLMetaclassCluster]
		ifFalse: [ next mapAndTrace: aMetaclass ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitMetaclassForTraits: aMetaclass [
	(self shouldSerializeBehaviorFully: aMetaclass)
		ifTrue: [
			self
				mapAndTraceByClusterName: aMetaclass
				to: FLMetaclassForTraitsCluster ]
		ifFalse: [ next mapAndTrace: aMetaclass ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitTrait: aTrait [
	(self shouldSerializeBehaviorFully: aTrait)
		ifTrue: [
			self
				mapAndTraceByClusterName: aTrait
				to: FLTraitCluster ]
		ifFalse: [ next mapAndTrace: aTrait ]
]

{ #category : 'visiting' }
FLFullGeneralMapper >> visitTraitedMetaclass: aMetaclass [
	(self shouldSerializeBehaviorFully: aMetaclass)
		ifTrue: [
			self
				mapAndTraceByClusterName: aMetaclass
				to: FLTraitedMetaclassCluster ]
		ifFalse: [ next mapAndTrace: aMetaclass ]
]
