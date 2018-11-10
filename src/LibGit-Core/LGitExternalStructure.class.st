"
Important: structs in libgit2 are usually freed by libgit2 itself.
	We therefore disable freeing of structs during GC to prevent invalid
	memory accesses. However, users of any struct need to make sure that
	they free the struct (using a manual #free send) iff it is necessary.
	Otherwise the structs will leak and we can't reclaim the space once the
	object representing the struct has been collected
"
Class {
	#name : #LGitExternalStructure,
	#superclass : #FFIExternalStructure,
	#traits : 'TLGitCalloutTrait + TLGitPrintingTrait',
	#classTraits : 'TLGitCalloutTrait classTrait + TLGitPrintingTrait classTrait',
	#category : #'LibGit-Core-Abstracts'
}

{ #category : #'managing accessors' }
LGitExternalStructure class >> createAccessorsFor: fieldName [ 
	"Define read/write accessors for the given field"
	| code |

	code := fieldName,'
	"This method was automatically generated"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self fieldAt: ' , fieldName printString.
	Author useAuthor: 'NativeBoost' during: [ 
		| selector |
		selector := self compileSilently: code classified: (self protocolForField: fieldName).
		self package addMethod: (self compiledMethodAt: selector) ].
	
	code := fieldName,': anObject
	"This method was automatically generated"
	<primitive: #primitiveNativeCall module: #NativeBoostPlugin error: errorCode>
	
	^ self fieldAt: ' , fieldName printString , ' put: anObject'.
	Author useAuthor: 'NativeBoost' during: [ 
		| selector |
		selector := self compileSilently: code classified: (self protocolForField: fieldName).
		self package addMethod: (self compiledMethodAt: selector) ]

]

{ #category : #'managing accessors' }
LGitExternalStructure class >> fieldNamesAndProtocols [
	^ Dictionary new
]

{ #category : #'managing accessors' }
LGitExternalStructure class >> fieldSpec [
	^ LGitExternalStructureFieldParser new  
		parseFields: self fieldsDesc structure: self.

]

{ #category : #'libgit-finalization' }
LGitExternalStructure class >> finalizeResourceData: aHandle [
	"Call the class specific free frunction if aHandle is still a valid external address."
	aHandle isNull ifTrue: [ ^self ].
	self perform: self freeFunctionSelector with: aHandle.
	aHandle beNull
]

{ #category : #'libgit-finalization' }
LGitExternalStructure class >> free: aHandle [
	aHandle free
]

{ #category : #'libgit-finalization' }
LGitExternalStructure class >> freeFunctionSelector [
	^ #free:
]

{ #category : #'instance creation' }
LGitExternalStructure class >> fromExternalAddress: anAddress [
	anAddress isNull
		ifTrue: [ ^ nil ].
	^ self fromHandle: anAddress getHandle
]

{ #category : #'managing accessors' }
LGitExternalStructure class >> generatedFieldProtocolName [
	^ 'libgit-fields'
]

{ #category : #'managing accessors' }
LGitExternalStructure class >> protocolForField: aString [
	^ self fieldNamesAndProtocols
		at: aString
		ifAbsent: [ 'libgit-fields' ]
]

{ #category : #'managing accessors' }
LGitExternalStructure class >> removeAccessor: aSelector [
	self package removeMethod: (self
		compiledMethodAt: aSelector
		ifAbsent: [ ^ self ]).
		
	^ super removeAccessor: aSelector
]

{ #category : #'initialize-release' }
LGitExternalStructure >> autoRelease [
	"Some LGit structures need specialised free functions. Use the same autoRelease functionality
	as FFIExternalReference to achieve this.

	Note, subclasses should implement #resourceData
	and #finalizeResourceData: on class side"
	FFIExternalResourceManager addResource: self
]

{ #category : #'initialize-release' }
LGitExternalStructure >> free [
	(handle isExternalAddress and: [ handle isNull ])
		ifTrue: [ 
			self class finalizeResourceData: handle.
			self assert: handle isNull ]
]

{ #category : #testing }
LGitExternalStructure >> isExternal [
	"answer true if data referenced by receiver located in external heap, or just in object memory "
	^ handle isExternalAddress
]

{ #category : #'initialize-release' }
LGitExternalStructure >> resourceData [
	^ self getHandle
]
