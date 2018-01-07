"
RG2MethodDefinition is a concrete representation of methods. It can be used to build browser for methods that are not in the image. It is polymorphic with CompiledMethod. 

* We can ask a RG2MethodDefinition for its selector using the selector message.
Example:
	(Point>>#dist:) asRing2Definition selector
		-> #dist

We can also ask the ring object representation of its class or the Smalltalk class actually implementing the corresponding compiledMethod. 

* To access the ring class definition name, use parentName
	aRG2MethodDefinition parentName
	
Example:
	(Point>>#dist:) asRing2Definition parentName
		->  #Point
		
* If you have a complete model where classes and methods are ring definition, to access the ring class definition , use parent
	aRG2MethodDefinition parent
	
Example:
	aRG2MethodDefinition(Point>>#dist:) parent
		->  aRG2ClassDefinition(Point)
		
* If you want to access the smalltalk class that contains the compiledMethod that is represented by a ringMethodDefinition, use realParent
	aRG2MethodDefinition realParent
	
Example:
	(Point>>#dist:) asRing2Definition realParent
		->  Point
		

Now a RingEntityDefinition offers two APIs: one that is generic and works for all the source code entities and this is the one we just 
presented: parent, parentName and realParent. Having such interface is important to build generic tools that could manipulate 
any entities in a polymorphic way (yes no isKindOf: everywhere).

In addition, a ring method definition offers a specific interface that should only be used when you know that you are solely manipulate
specific entity such as class element: method definition, class comment, and variables. 

Here is the equivalence table

	realParent 				realClass
	parent					ringClass
	parentName			className


* The message class returns the class of the object :).

Example:
	(Point>>#dist:) asRing2Definition class
		->  RingMethodDefinition
		
* The message className returns the name of the ring class defining the reingMethodDefinition.

Example:
	(Point>>#dist:) asRing2Definition className
		->  #Point		
		
* If you have a complete model where classes and methods are ring definition, to access the ring class definition , use parent
	aRG2MethodDefinition ringClass
	
Example:
	aRG2MethodDefinition(Point>>#dist:) ringClass
		->  aRG2ClassDefinition(Point)
		
		
* If you want to access the smalltalk class that contains the compiledMethod that is represented by a ringMethodDefinition, use realClass
	aRG2MethodDefinition realClass
	
Example:
	(Point>>#dist:) asRing2Definition realClass
		->  Point


"
Class {
	#name : #RG2MethodDefinition,
	#superclass : #RG2ElementDefinition,
	#instVars : [
		'protocol',
		'sourceCode',
		'package',
		'author',
		'time',
		'tags'
	],
	#category : #'Ring2-Core-Kernel'
}

{ #category : #'managing container' }
RG2MethodDefinition >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent pvtAddLocalMethod: self.
	
	

]

{ #category : #accessing }
RG2MethodDefinition >> argumentNames [		

	^ self ast argumentNames
]

{ #category : #accessing }
RG2MethodDefinition >> ast [
	"Answer my AST with semantic analysis. See #parseTree."
	
	^ self propertyNamed: #ast ifAbsentPut: [ self parseTree doSemanticAnalysisIn: self methodClass ]


]

{ #category : #private }
RG2MethodDefinition >> astFromSource [

	^ RBParser parseMethod: self sourceCode
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> author [

	^ self backend forBehavior authorFor: self
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> author: aString [

	self backend forBehavior setAuthorFor: self to: aString
]

{ #category : #'backward compatibility' }
RG2MethodDefinition >> category [

	^ self protocol
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> cleanTags [

	self backend forMethod cleanMethodTagsFor: self.
	
	"todo: announce"
]

{ #category : #'as yet unclassified' }
RG2MethodDefinition >> compiledMethod [
	^ self
]

{ #category : #'default model values' }
RG2MethodDefinition >> defaultAuthor [

	^ ''
]

{ #category : #'managing container' }
RG2MethodDefinition >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : #'default model values' }
RG2MethodDefinition >> defaultSourceCode [

	^ self sourceCodeForNoSelector
]

{ #category : #'default model values' }
RG2MethodDefinition >> defaultTags [

	^ Set new
]

{ #category : #'default model values' }
RG2MethodDefinition >> defaultTime [

	^ DateAndTime new
]

{ #category : #accessing }
RG2MethodDefinition >> fullName [
	"Keeps a unique description for the receiver. As annotation to avoid converting each time is invoked"
	
	^ (self parent name, '>>#', self selector) asSymbol
]

{ #category : #accessing }
RG2MethodDefinition >> hasSourceCode [

	^ self backend forMethod hasSourceCodeFor: self
]

{ #category : #initialization }
RG2MethodDefinition >> initialize [

	super initialize.

	sourceCode := self unresolvedValue: self defaultSourceCode.
	tags := self unresolvedValue: self defaultTags.	"tags must be set before package"
	package := self unresolvedValue: self parent package.
	author := self unresolvedValue: self defaultAuthor.
	time := self unresolvedValue: self defaultTime.
	
]

{ #category : #initialization }
RG2MethodDefinition >> initializeUnresolved [

	super initializeUnresolved.

	sourceCode := self unresolvedValue: self defaultSourceCode.
	package := self unresolvedValue: self parent package.
	author := self unresolvedValue: self defaultAuthor.
	time := self unresolvedValue: self defaultTime.
	tags := self unresolvedValue: self defaultTags.	
	
]

{ #category : #accessing }
RG2MethodDefinition >> isExtension [
	"The receiver is an extension when is defined in a different package to the one of its parent"
	
	^ self parent package ~= self package
]

{ #category : #testing }
RG2MethodDefinition >> isFromTrait [
	^ self parent isTrait
]

{ #category : #testing }
RG2MethodDefinition >> isMetaSide [

	^ self parent isMeta
]

{ #category : #testing }
RG2MethodDefinition >> isMethod [

	^true
]

{ #category : #'queries - tags' }
RG2MethodDefinition >> isTaggedWith: aSymbol [

	^self tags includes: aSymbol 
]

{ #category : #resolving }
RG2MethodDefinition >> makeResolved [

	super makeResolved.
	
	protocol := self protocol.
	sourceCode := self sourceCode.
	package := self package.	
	author := self author.	
	time := self time.	
	tags := self tags.
]

{ #category : #'queries - class' }
RG2MethodDefinition >> methodClass [

	^ self parent
]

{ #category : #accessing }
RG2MethodDefinition >> numArgs [
	^ self selector asString numArgs
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> package [

	^ self backend forMethod methodPackageFor: self
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> package: aRG2PackageDefinition [

	| oldPackage |
	
	oldPackage := self package.
	self backend forMethod setMethodPackageFor: self to: aRG2PackageDefinition.
	self environment addPackage: aRG2PackageDefinition.
	(self parent package = aRG2PackageDefinition)
		ifFalse: [ 
			oldPackage isRing2Resolved 
				ifTrue: [ oldPackage removeExtensionMethod: self ].
			self package addExtensionMethod: self ].

]

{ #category : #accessing }
RG2MethodDefinition >> parseTree [

	^(RBParser 
		parseMethod: self sourceCode 
		onError: [ :msg :pos | ^ nil ]) methodClass: self methodClass.

]

{ #category : #printing }
RG2MethodDefinition >> printOn: aStream [ 

	self parent name ifNotNil: [
		aStream nextPutAll: self parent name;
				  nextPutAll: '>>' ].
	aStream print: self selector
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> protocol [

	| methodTags |
	
	methodTags := self tags.
	^	methodTags
		ifEmpty: [  self class asYetUnclassifiedProtocolName]
		ifNotEmpty: [ methodTags sorted first ]

]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> protocol: aSymbol [

	"for compatiblity with the old protocols"

	self cleanTags.
	self tagWith: aSymbol.
	
	
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtAuthor [

	^ author value
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtAuthor: aString [

	^ author := aString
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtCleanTags [

	tags := self defaultTags.
		
	"TODO:Announce if not empty"
	
	
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtPackage [

	^ package value
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtPackage: aRG2PackageDefinition [

	self environment verifyOwnership: aRG2PackageDefinition.

	^ package := aRG2PackageDefinition
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtResolvableProperties [

	^ super pvtResolvableProperties, { 
		#protocol -> protocol. 
		#sourceCode -> sourceCode. 
		#package -> package. 
		#author -> author. 
		#time -> time. 
		#tags -> tags. 
	}
	
	
]

{ #category : #private }
RG2MethodDefinition >> pvtSafeSourceCode [

	| aStringOrUnresolved |
	aStringOrUnresolved := self pvtSourceCode. 
	^ aStringOrUnresolved isRing2Resolved
		ifFalse: [ 
			self pvtName isRing2Resolved 
				ifTrue: [ self sourceCodeForNoSource ]
				ifFalse: [ self sourceCodeForNoSelector ] ]
		ifTrue: [ aStringOrUnresolved value ]
]

{ #category : #private }
RG2MethodDefinition >> pvtSourceCode [

	^ sourceCode 
]

{ #category : #private }
RG2MethodDefinition >> pvtSourceCode: anObject [

	sourceCode := anObject
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtTagWith: aSymbol [
	
	tags isRing2Resolved ifFalse: [ 
		self pvtCleanTags  ].
	
	tags add: aSymbol.	
	
]

{ #category : #private }
RG2MethodDefinition >> pvtTagsDo: aBlock [

	^ tags value do: aBlock
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtTime [

	^ time value
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtTime: aDateAndTime [

	^ time := aDateAndTime
]

{ #category : #'private - backend interface' }
RG2MethodDefinition >> pvtUntagFrom: aSymbol [
	
	(tags value includes: aSymbol)
		ifTrue: [ tags remove: aSymbol ].
	
	"TODO:Announce"
	
	
]

{ #category : #removing }
RG2MethodDefinition >> removeFromSystem [

	^ self parent removeLocalMethod: self
]

{ #category : #accessing }
RG2MethodDefinition >> selector [
	"Retrieves the name of the method"

	^ self name asSymbol
]

{ #category : #accessing }
RG2MethodDefinition >> sourceCode [

	^ self backend forMethod sourceCodeFor: self
]

{ #category : #accessing }
RG2MethodDefinition >> sourceCode: anObject [

	"ATTENTION: There is no check here if the selector is changed!"

	self backend forMethod setSourceCodeFor: self to: anObject asString
	"TODO: announcements"
]

{ #category : #accessing }
RG2MethodDefinition >> sourceCodeForNoSelector [

	^ 'unresolvedMessage', String cr, String tab, '"source code for the method model not set"'
]

{ #category : #accessing }
RG2MethodDefinition >> sourceCodeForNoSource [

	^ self selector asMethodPreamble, String cr, String tab, '"source code for the method model not set"'
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> tagWith: aSymbol [ 

	self backend forMethod tagMethod: self with: aSymbol.
	
	self parent addMethodTag: aSymbol.
	
	"todo:announce"
]

{ #category : #'accessing - model' }
RG2MethodDefinition >> tags [

	| allTags |
	allTags := IdentitySet new.
	self tagsDo: [ :each | allTags add: each].
	^ allTags asArray

]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> tagsDo: aBlock [

	self backend forMethod tagsForMethod: self do: aBlock
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> time [

	^ self backend forBehavior timeFor: self
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> time: aDateAndTime [

	self backend forBehavior setTimeFor: self to: aDateAndTime
]

{ #category : #'accessing - backend' }
RG2MethodDefinition >> untagFrom: aSymbol [

	self backend forMethod untagMethod: self from: aSymbol
]
