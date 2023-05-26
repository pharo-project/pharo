Class {
	#name : #PBInstanceVariableMapping,
	#superclass : #Object,
	#instVars : [
		'environment',
		'contextPcIndex',
		'contextReceiverIndex',
		'processContextIndex',
		'classMethodDictionaryIndex',
		'classSuperclassIndex',
		'processNameIndex',
		'traitClassTraitIndex',
		'traitMethodDictionaryIndex',
		'contextSenderIndex',
		'classPoolIndex',
		'contextStackpIndex',
		'contextMethodIndex',
		'classNameIndex',
		'processSchedulerActiveProcessIndex',
		'processPriorityIndex',
		'contextClosureOrNilIndex',
		'classFormatIndex',
		'processPriorityListIndex',
		'processNextLinkIndex',
		'traitNameIndex',
		'smalltalkImageGlobalsIndex',
		'classSharedPoolsIndex',
		'classNumberOfVariables',
		'classInstanceVariableNamesIndex',
		'dictionaryArrayIndex',
		'listOfClassSlots'
	],
	#category : 'Pharo30Bootstrap'
}

{ #category : #'instance-creation' }
PBInstanceVariableMapping class >> onEnvironment: aRingEnvironment [ 
	^ self new
		environment: aRingEnvironment;
		initializeIndexes;
		yourself.
]

{ #category : #initialization }
PBInstanceVariableMapping >> allSlotsOf: aClassName [

	^ ((environment classNamed: aClassName) allSlots collect: [ :e | e name ]) joinUsing: ' '.
]

{ #category : #accessing }
PBInstanceVariableMapping >> classFormatIndex [
	^ classFormatIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> classInstanceVariableNamesIndex [
	^ classInstanceVariableNamesIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> classMethodDictionaryIndex [
	^ classMethodDictionaryIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> classNameIndex [
	^ classNameIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> classNumberOfVariables [
	^ classNumberOfVariables
]

{ #category : #accessing }
PBInstanceVariableMapping >> classPoolIndex [
	^ classPoolIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> classSharedPoolsIndex [
	^ classSharedPoolsIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> classSuperclassIndex [
	^ classSuperclassIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> contextClosureOrNilIndex [
	^ contextClosureOrNilIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> contextMethodIndex [
	^ contextMethodIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> contextPcIndex [
	^ contextPcIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> contextReceiverIndex [
	^ contextReceiverIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> contextSenderIndex [
	^ contextSenderIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> contextStackpIndex [
	^ contextStackpIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> dictionaryArrayIndex [
	^ dictionaryArrayIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> environment [
	^ environment
]

{ #category : #accessing }
PBInstanceVariableMapping >> environment: aRingEnvironment [ 
	environment := aRingEnvironment
]

{ #category : #initialization }
PBInstanceVariableMapping >> initializeIndexes [
	contextPcIndex := self lookup: #pc in: #Context.
	contextReceiverIndex := self lookup: #receiver in: #Context.
	processContextIndex := self lookup: #suspendedContext in: #Process.
	classMethodDictionaryIndex := self lookup: #methodDict in: #Class.
	classSuperclassIndex := self lookup: #superclass in: #Class.
	processNameIndex := self lookup: #name in: #Process.
	traitClassTraitIndex := self lookup: #classTrait in: #Trait.
	traitMethodDictionaryIndex := self lookup: #methodDict in: #Trait.
	contextSenderIndex := self lookup: #sender in: #Context.
	classPoolIndex := self lookup: #classPool in: #Class.
	contextStackpIndex := self lookup: #stackp in: #Context.
	contextMethodIndex := self lookup: #method in: #Context.
	classNameIndex := self lookup: #name in: #Class.
	processSchedulerActiveProcessIndex := self lookup: #activeProcess in: #ProcessorScheduler.
	processPriorityIndex := self lookup: #priority in: #Process.
	contextClosureOrNilIndex := self lookup: #closureOrNil in: #Context.
	classFormatIndex := self lookup: #format in: #Class.
	processPriorityListIndex := self lookup: #myList in: #Process.
	processNextLinkIndex := self lookup: #nextLink in: #Process.
	traitNameIndex := self lookup: #name in: #Trait.

	smalltalkImageGlobalsIndex := self lookup: #globals in: #SmalltalkImage.
	classSharedPoolsIndex := self lookup: #sharedPools in: #Class.
	classNumberOfVariables := self numberOfInstanceVariablesOf: #Class.
	classInstanceVariableNamesIndex := self lookup: #instanceVariableNames in: #Class.
	dictionaryArrayIndex := self lookup: #array in: #Dictionary.
	
	listOfClassSlots := self allSlotsOf: #Class.
]

{ #category : #accessing }
PBInstanceVariableMapping >> listOfClassSlots [
	^ listOfClassSlots
]

{ #category : #initialization }
PBInstanceVariableMapping >> lookup: anInstanceVariable in: aClass [
	| slot |
	
	"I lookup the indices in the RGEnvironment, if one of the classes does not exists (like Traits) I put nil, I should not explode. 
	If it explode we have a problem."
	slot := (environment ask behaviorNamed: aClass ifAbsent: [ ^nil ]) allSlots
		detect: [ :x | x name = anInstanceVariable ]
		ifNone: [ ^ nil ].
	^ slot index
]

{ #category : #initialize }
PBInstanceVariableMapping >> numberOfInstanceVariablesOf: aClassName [ 
	^ (environment classNamed: aClassName) allSlots size
]

{ #category : #accessing }
PBInstanceVariableMapping >> processContextIndex [
	^ processContextIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> processNameIndex [
	^ processNameIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> processNextLinkIndex [
	^ processNextLinkIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> processPriorityIndex [
	^ processPriorityIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> processPriorityListIndex [
	^ processPriorityListIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> processSchedulerActiveProcessIndex [
	^ processSchedulerActiveProcessIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> smalltalkImageGlobalsIndex [
	^ smalltalkImageGlobalsIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> traitClassTraitIndex [
	^ traitClassTraitIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> traitMethodDictionaryIndex [
	^ traitMethodDictionaryIndex
]

{ #category : #accessing }
PBInstanceVariableMapping >> traitNameIndex [
	^ traitNameIndex
]
