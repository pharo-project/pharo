Class {
	#name : 'OCMethodInstaller',
	#superclass : 'Object',
	#instVars : [
		'priorMethod',
		'changeStamp',
		'logged',
		'protocol',
		'sourceManager'
	],
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'installing' }
OCMethodInstaller >> addAndClassifyMethod: compiledMethod into: behavior [

	behavior addSelectorSilently: compiledMethod selector withMethod: compiledMethod
]

{ #category : 'accessing' }
OCMethodInstaller >> changeStamp [

	^ changeStamp
]

{ #category : 'accessing' }
OCMethodInstaller >> changeStamp: anObject [

	changeStamp := anObject
]

{ #category : 'configuring' }
OCMethodInstaller >> forExtendedBehavior [

	^ OCBehaviorWithExtensionInstaller new
		behaviorInstaller: self;
		yourself
]

{ #category : 'initialization' }
OCMethodInstaller >> initialize [

	super initialize.
	sourceManager := SourcesManager current
]

{ #category : 'installing' }
OCMethodInstaller >> install: method into: behavior compiler: compiler [
	"Compile and install a method in a class"

	| logSource |
	self priorMethod ifNil: [ "In case the method comes from a trait, we ignore it because it means we are overriding the trait method in the class"
		behavior compiledMethodAt: method selector ifPresent: [ :aMethod | aMethod isFromTrait ifFalse: [ self priorMethod: aMethod ] ] ]. "In case we are not compiling the method for the first time, we want to ensure that some properties are kept."
	self priorMethod ifNotNil: [ self priorMethod migratePersistingPropertiesIn: method ].

	logSource := self logged.
	protocol := protocol ifNil: [ self priorMethod ifNotNil: [ self priorMethod protocol name ] ifNil: [ Protocol unclassified ] ].

	logSource ifTrue: [
			sourceManager
				writeSource: compiler source
				of: method
				withFooter: ''
				protocol: self protocolName
				timestamp: self changeStamp printString
				priorSourcePointer: self priorSourcePointer ].
			
	"Install the method and classify AFTER setting the source pointer.
	If the method is a trait method, trait installation is hooked down here.
	And it depends on having set the source pointer before.
	
	This is not the ideal order, because this means that a failure in writing the source code may prevent method installation in the system (and thus preventing further work/updates to code)."
	self addAndClassifyMethod: method into: behavior.
	method cachePragmas.

	^ method
]

{ #category : 'accessing' }
OCMethodInstaller >> logged [

	^ logged ifNil: [ true ]
]

{ #category : 'accessing' }
OCMethodInstaller >> logged: anObject [

	logged := anObject
]

{ #category : 'accessing' }
OCMethodInstaller >> priorMethod [

	^ priorMethod
]

{ #category : 'accessing' }
OCMethodInstaller >> priorMethod: anObject [

	priorMethod := anObject
]

{ #category : 'installing' }
OCMethodInstaller >> priorSourcePointer [

	^ self priorMethod ifNil: [ 0 ] ifNotNil: [ self priorMethod sourcePointer ]
]

{ #category : 'accessing' }
OCMethodInstaller >> protocol [

	^ protocol
]

{ #category : 'accessing' }
OCMethodInstaller >> protocol: anObject [

	protocol := anObject
]

{ #category : 'installing' }
OCMethodInstaller >> protocolName [
	"Return a string representation of the protocol, to store it in source files"

	^ (protocol isString
		   ifTrue: [ protocol ]
		   ifFalse: [ protocol name ]) asString
]
