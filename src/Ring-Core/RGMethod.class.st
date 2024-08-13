"
RG2Method is a concrete representation of methods. It can be used to build browser for methods that are not in the image. It is polymorphic with CompiledMethod. 

* We can ask a RG2Method for its selector using the selector message.
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
	#name : 'RGMethod',
	#superclass : 'RGElement',
	#instVars : [
		'sourceCode',
		'package',
		'author',
		'time',
		'tags'
	],
	#category : 'Ring-Core-Kernel',
	#package : 'Ring-Core',
	#tag : 'Kernel'
}

{ #category : 'managing container' }
RGMethod >> addoptToParentStub [

	super addoptToParentStub.
	self environment backend createUnresolvedClassGroupFor: self parent.
	self parent pvtAddLocalMethod: self
]

{ #category : 'accessing' }
RGMethod >> argumentNames [

	^ self ast argumentNames
]

{ #category : 'accessing' }
RGMethod >> ast [
	"Answer my AST with semantic analysis. See #parseTree."

	^ self propertyNamed: #ast ifAbsentPut: [ self parseTree doSemanticAnalysisIn: self methodClass ]
]

{ #category : 'private' }
RGMethod >> astFromSource [

	^ RBParser parseMethod: self sourceCode
]

{ #category : 'accessing - backend' }
RGMethod >> author [

	^ self backend forBehavior authorFor: self
]

{ #category : 'accessing - backend' }
RGMethod >> author: aString [

	self backend forBehavior setAuthorFor: self to: aString
]

{ #category : 'backward compatibility' }
RGMethod >> category [

	^ self protocol
]

{ #category : 'private' }
RGMethod >> changeProtocolDuring: aBlock [

	| oldProtocol |

	oldProtocol := self protocol.
	aBlock value.
	self announce: (MethodRecategorized method: self oldProtocol: oldProtocol)
]

{ #category : 'accessing - backend' }
RGMethod >> cleanTags [

	self changeProtocolDuring: [
		self cleanTagsWithoutAnnouncemnt ]
]

{ #category : 'accessing - backend' }
RGMethod >> cleanTagsWithoutAnnouncemnt [

	self backend forMethod cleanMethodTagsFor: self
]

{ #category : 'accessing' }
RGMethod >> compiledMethod [
	^ self
]

{ #category : 'default model values' }
RGMethod >> defaultAuthor [

	^ ''
]

{ #category : 'managing container' }
RGMethod >> defaultParentStub [

	^ self defaultClassStub
]

{ #category : 'default model values' }
RGMethod >> defaultSourceCode [

	^ self sourceCodeForNoSelector
]

{ #category : 'default model values' }
RGMethod >> defaultTags [

	^ Set new
]

{ #category : 'default model values' }
RGMethod >> defaultTime [

	^ DateAndTime new
]

{ #category : 'accessing' }
RGMethod >> fullName [
	"Keeps a unique description for the receiver. As annotation to avoid converting each time is invoked"

	^ (self parent name, '>>#', self selector) asSymbol
]

{ #category : 'accessing' }
RGMethod >> hasSourceCode [

	^ self backend forMethod hasSourceCodeFor: self
]

{ #category : 'initialization' }
RGMethod >> initialize [

	super initialize.

	sourceCode := self unresolvedValue: self defaultSourceCode.
	tags := self unresolvedValue: self defaultTags.	"tags must be set before package"
	package := self unresolvedValue: self parent package.
	author := self unresolvedValue: self defaultAuthor.
	time := self unresolvedValue: self defaultTime
]

{ #category : 'initialization' }
RGMethod >> initializeUnresolved [

	super initializeUnresolved.

	sourceCode := self unresolvedValue: self defaultSourceCode.
	tags := self unresolvedValue: self defaultTags.
	package := self unresolvedValue: self parent package.
	author := self unresolvedValue: self defaultAuthor.
	time := self unresolvedValue: self defaultTime
]

{ #category : 'testing' }
RGMethod >> isClassified [

	^ self protocol ~= Protocol unclassified
]

{ #category : 'accessing' }
RGMethod >> isExtension [
	"The receiver is an extension when is defined in a different package to the one of its parent"

	^ self parent package ~= self package
]

{ #category : 'testing' }
RGMethod >> isFromTrait [
	^ self parent isTrait
]

{ #category : 'testing' }
RGMethod >> isLiteralMethod [
	"Ring methods does not know how to detect if they are literal"
	^ false
]

{ #category : 'testing' }
RGMethod >> isMeta [

	^ self parent isMeta
]

{ #category : 'testing' }
RGMethod >> isMetaSide [
	self
		deprecated: 'Use isMeta instead'
		transformWith: '`@receiver isMetaSide' -> '`@receiver isMeta'.
	^ self isMeta
]

{ #category : 'testing' }
RGMethod >> isMethod [

	^true
]

{ #category : 'queries - tags' }
RGMethod >> isTaggedWith: aSymbol [

	^self tags includes: aSymbol
]

{ #category : 'resolving' }
RGMethod >> makeResolved [

	super makeResolved.

	sourceCode := self sourceCode markAsRingResolved.
	package := self package markAsRingResolved.
	author := self author markAsRingResolved.
	time := self time markAsRingResolved.
	tags := self tags markAsRingResolved
]

{ #category : 'queries - class' }
RGMethod >> methodClass [

	^ self parent
]

{ #category : 'accessing' }
RGMethod >> numArgs [
	^ self selector asString numArgs
]

{ #category : 'accessing - backend' }
RGMethod >> package [

	^ self backend forMethod methodPackageFor: self
]

{ #category : 'accessing - backend' }
RGMethod >> package: anRGPackage [

	self backend forMethod setMethodPackageFor: self to: anRGPackage.
	self environment addPackage: anRGPackage.
	(self parent package = anRGPackage)
		ifFalse: [ self package addExtensionMethod: self ]
]

{ #category : 'accessing' }
RGMethod >> parseTree [
	| tree |
	tree := RBParser
		  parseMethod: self sourceCode
		  onError: [ :str :pos | ^ nil ].
	tree ifNotNil: [ tree doSemanticAnalysis ].
	^ tree
]

{ #category : 'printing' }
RGMethod >> printOn: aStream [

	self parent name ifNotNil: [
		aStream nextPutAll: self parent name;
				  nextPutAll: '>>' ].
	aStream print: self selector
]

{ #category : 'accessing - backend' }
RGMethod >> protocol [

	| methodTags |

	methodTags := self tags.
	^	methodTags
		ifEmpty: [  self class asYetUnclassifiedProtocolName]
		ifNotEmpty: [ methodTags sorted first ]
]

{ #category : 'accessing - backend' }
RGMethod >> protocol: aSymbol [

	self cleanTagsWithoutAnnouncemnt.
	self tagWith: aSymbol
]

{ #category : 'accessing - backend' }
RGMethod >> protocolName [

	^ self protocol
]

{ #category : 'private - backend interface' }
RGMethod >> pvtAuthor [

	^ author value
]

{ #category : 'private - backend interface' }
RGMethod >> pvtAuthor: aString [

	^ author := aString
]

{ #category : 'private - backend interface' }
RGMethod >> pvtCleanTags [

	tags := self defaultTags.

	"TODO:Announce if not empty"
]

{ #category : 'private - backend interface' }
RGMethod >> pvtPackage [

	^ package value
]

{ #category : 'private - backend interface' }
RGMethod >> pvtPackage: anRGPackageDefinition [

	self environment verifyOwnership: anRGPackageDefinition.

	^ package := anRGPackageDefinition
]

{ #category : 'private - backend interface' }
RGMethod >> pvtResolvableProperties [

	^ super pvtResolvableProperties, {
		#sourceCode -> sourceCode.
		#package -> package.
		#author -> author.
		#time -> time.
		#tags -> tags.
	}
]

{ #category : 'private' }
RGMethod >> pvtSafeSourceCode [

	| aStringOrUnresolved |
	aStringOrUnresolved := self pvtSourceCode.
	^ aStringOrUnresolved isRingResolved
		ifFalse: [
			self pvtName isRingResolved
				ifTrue: [ self sourceCodeForNoSource ]
				ifFalse: [ self sourceCodeForNoSelector ] ]
		ifTrue: [ aStringOrUnresolved value ]
]

{ #category : 'private' }
RGMethod >> pvtSourceCode [

	^ sourceCode
]

{ #category : 'private' }
RGMethod >> pvtSourceCode: anObject [

	sourceCode := anObject
]

{ #category : 'private - backend interface' }
RGMethod >> pvtTagWith: aSymbol [

	tags isRingResolved ifFalse: [
		self pvtCleanTags  ].

	tags add: aSymbol
]

{ #category : 'private' }
RGMethod >> pvtTagsDo: aBlock [

	^ tags value do: aBlock
]

{ #category : 'private - backend interface' }
RGMethod >> pvtTime [

	^ time value
]

{ #category : 'private - backend interface' }
RGMethod >> pvtTime: aDateAndTime [

	^ time := aDateAndTime
]

{ #category : 'private - backend interface' }
RGMethod >> pvtUntagFrom: aSymbol [

	(tags value includes: aSymbol)
		ifTrue: [ tags remove: aSymbol ].

	"TODO:Announce"
]

{ #category : 'removing' }
RGMethod >> removeFromSystem [

	^ self parent removeLocalMethod: self
]

{ #category : 'accessing' }
RGMethod >> selector [
	"Retrieves the name of the method"

	^ self name asSymbol
]

{ #category : 'accessing' }
RGMethod >> sourceCode [

	^ self backend forMethod sourceCodeFor: self
]

{ #category : 'accessing' }
RGMethod >> sourceCode: anObject [

	"ATTENTION: There is no check here if the selector is changed!"

	self backend forMethod setSourceCodeFor: self to: anObject asString
	"TODO: announcements"
]

{ #category : 'accessing' }
RGMethod >> sourceCodeForNoSelector [

	^ 'unresolvedMessage', String cr, String tab, '"source code for the method model not set"'
]

{ #category : 'accessing' }
RGMethod >> sourceCodeForNoSource [

	^ self selector asMethodPreamble, String cr, String tab, '"source code for the method model not set"'
]

{ #category : 'accessing - backend' }
RGMethod >> tagWith: aSymbol [

	self changeProtocolDuring: [
		self backend forMethod tagMethod: self with: aSymbol.
		self parent addProtocol: aSymbol ]
]

{ #category : 'accessing - model' }
RGMethod >> tags [

	| allTags |
	allTags := IdentitySet new.
	self tagsDo: [ :each | allTags add: each].
	^ allTags asArray
]

{ #category : 'accessing - backend' }
RGMethod >> tagsDo: aBlock [

	self backend forMethod tagsForMethod: self do: aBlock
]

{ #category : 'accessing - backend' }
RGMethod >> time [

	^ self backend forBehavior timeFor: self
]

{ #category : 'accessing - backend' }
RGMethod >> time: aDateAndTime [

	self backend forBehavior setTimeFor: self to: aDateAndTime
]

{ #category : 'accessing - backend' }
RGMethod >> untagFrom: aSymbol [

	self backend forMethod untagMethod: self from: aSymbol
]
