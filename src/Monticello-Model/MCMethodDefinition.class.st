"
A MCMethodDefinition represents a method definition. 
It captures the following information.

Instance Variables
	category:		<Object>
	classIsMeta:		<Object>
	className:		<Object>
	selector:		<Object>
	source:		<Object>
	timeStamp:		<Object>


"
Class {
	#name : 'MCMethodDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'classIsMeta',
		'source',
		'category',
		'selector',
		'className',
		'timeStamp',
		'sortKey'
	],
	#classVars : [
		'Definitions',
		'InitializersEnabled'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'initialization' }
MCMethodDefinition class >> cachedDefinitions [

	^ Definitions ifNil: [ Definitions := WeakIdentityKeyDictionary new ]
]

{ #category : 'instance creation' }
MCMethodDefinition class >> className: classString classIsMeta: metaBoolean selector: selectorString category: catString timeStamp: timeString source: sourceString [
	^ self new
		initializeWithClassName: classString
		classIsMeta: metaBoolean
		selector: selectorString
		category: catString
		timeStamp: timeString
		source: sourceString
]

{ #category : 'instance creation' }
MCMethodDefinition class >> className: classString
selector: selectorString
category: catString
timeStamp: timeString
source: sourceString [
	^ self	className: classString
			classIsMeta: false
			selector: selectorString
			category: catString
			timeStamp: timeString
			source: sourceString
]

{ #category : 'cleanup' }
MCMethodDefinition class >> cleanUp [
	"Flush caches"

	self shutDown.
]

{ #category : 'initialization' }
MCMethodDefinition class >> flushMethodCache [
	"We do not named this method flushCache because it would override an important class methods."
	Definitions := nil.
]

{ #category : 'class initialization' }
MCMethodDefinition class >> initialize [

	SessionManager default registerSystemClassNamed: self name
]

{ #category : 'settings' }
MCMethodDefinition class >> initializersEnabled [

	^ InitializersEnabled ifNil: [true]
]

{ #category : 'settings' }
MCMethodDefinition class >> initializersEnabled: aBoolean [

	InitializersEnabled := aBoolean
]

{ #category : 'system startup' }
MCMethodDefinition class >> shutDown [
	"Free up all cached monticello method definitions"
	self flushMethodCache 
]

{ #category : 'comparing' }
MCMethodDefinition >> = aDefinition [
	^ super = aDefinition
		and: [ aDefinition protocol = self protocol
		and: [ aDefinition source withInternalLineEndings = self source withInternalLineEndings ] ]
]

{ #category : 'visiting' }
MCMethodDefinition >> accept: aVisitor [
	^ aVisitor visitMethodDefinition: self
]

{ #category : 'accessing' }
MCMethodDefinition >> actualClass [

	^ self class environment at: className ifPresent: [ :class |
		  classIsMeta
			  ifTrue: [ class classSide ]
			  ifFalse: [ class ] ]
]

{ #category : 'accessing - backward' }
MCMethodDefinition >> category [
	"Please favor protocol instead of category. We want to have method protocol and class package and tag = a category"
	^ category
]

{ #category : 'accessing' }
MCMethodDefinition >> classIsMeta [
	^ classIsMeta
]

{ #category : 'accessing' }
MCMethodDefinition >> className [
	^className
]

{ #category : 'printing' }
MCMethodDefinition >> description [
	^ Array	
		with: className
		with: selector
		with: classIsMeta
]

{ #category : 'accessing' }
MCMethodDefinition >> diffSource [
	^'"protocol: ', self protocol,'"
', self source.
]

{ #category : 'printing' }
MCMethodDefinition >> fullClassName [
	"Using #class selector for classes for backwards compatibility"

	^ self classIsMeta
		ifFalse: [self className]
		ifTrue: [
			(self actualClass isNil or: [ self actualClass isTrait ])
				ifFalse: [self className, ' class']
				ifTrue: [self className, ' classSide']]
]

{ #category : 'comparing' }
MCMethodDefinition >> hash [
	| hash |
	hash := String stringHash: classIsMeta asString initialHash: 0.
	hash := String stringHash: source initialHash: hash.
	hash := String stringHash: category initialHash: hash.
	hash := String stringHash: className initialHash: hash.
	^ hash
]

{ #category : 'serializing' }
MCMethodDefinition >> initializeWithClassName: classString classIsMeta: metaBoolean selector: selectorString category: protocolName timeStamp: timeString source: sourceString [

	className := classString asSymbol.
	selector := selectorString asSymbol.
	category := protocolName isEmptyOrNil ifTrue: [ Protocol unclassified ] ifFalse: [ protocolName asSymbol ].
	timeStamp := timeString.
	classIsMeta := metaBoolean.
	source := sourceString withInternalLineEndings
]

{ #category : 'installing' }
MCMethodDefinition >> isExtensionMethod [
	^ category beginsWith: '*'
]

{ #category : 'installing' }
MCMethodDefinition >> isExternalStructureFieldDefinition [
	"Really belongs in the FFI package, for now, include in base even if FFI is not loaded due to the dire consequences if its not here and an FFI package is loaded (incorrect field compilation can cause VM crashes).
	
	If in the future other external packages are found to need custom post-load blocks as well, a pluggable way of doing so should be investigated, but for now it's not worth the effort and coordination required with external package maintainers"

	^ selector = #fields and: [
		  classIsMeta and: [
			  (self class environment at: #ExternalStructure ifPresent: [ :externalStructure | self actualClass instanceSide inheritsFrom: externalStructure ]) == true ] ]
]

{ #category : 'testing' }
MCMethodDefinition >> isInitializer [
	^ selector = #initialize and: [classIsMeta]
	
]

{ #category : 'testing' }
MCMethodDefinition >> isLoadable [
	^self actualClass isNotNil
]

{ #category : 'testing' }
MCMethodDefinition >> isMethodDefinition [
	^true
]

{ #category : 'installing' }
MCMethodDefinition >> isOverrideMethod [
	"this oughta check the package"
	^ self isExtensionMethod and: [category endsWith: '-override']
]

{ #category : 'installing' }
MCMethodDefinition >> load [

	| method |
	method := self actualClass compiler
		          protocol: category;
		          changeStamp: timeStamp;
		          permitUndeclared: true;
		          install: source.

	method selector = selector ifTrue: [ ^ self ].
	"This can happen if we provide a selector that does not match the source code. I (Cyril Ferlicot) personnaly do not like that. I would prefer even that the MCMethodDefinition would not have a selector variable at all. But this is used in some places of the system :("
	Warning signal: ('Monticello: Selector missmatch. Got {1}, expected {2} for class {3}.' format: { method selector. selector. self actualClass name }).
	selector := method selector
]

{ #category : 'installing' }
MCMethodDefinition >> overridenMethodOrNil [
	"Answer the method that I've overriden on load. 

	Preconditions: 
	  - self actualClass is installed in the System Dictionary.
	  - self isOverrideMethod is true."
	
	| realMethod |
	realMethod := self actualClass
		compiledMethodAt: self selector
		ifAbsent: [ ^ nil ].
		
	^ SourceFiles
		changeRecordsFor: realMethod
		detect: [ :protocol | protocol ~= category ]
]

{ #category : 'installing' }
MCMethodDefinition >> postloadOver: aDefinition [
	super postloadOver: aDefinition.
	self class initializersEnabled ifTrue: [
		(self isInitializer
			and: [ self actualClass isTrait not 
					and: [ aDefinition isNil or: [ self source ~= aDefinition source ]]]) ifTrue: [
				self actualClass instanceSide initialize ] ].
	"Postloading of FFI fields. This code will be called when loading FFI structures that are not by default in the image. This is NOT dead code."
	self isExternalStructureFieldDefinition
		ifTrue: [self actualClass instanceSide compileFields].
]

{ #category : 'accessing' }
MCMethodDefinition >> protocol [
	"Return in which protocol (conceptual groups of methods) the receiver is grouped into."
	^ category
]

{ #category : 'installing' }
MCMethodDefinition >> removeSelector: aSelector fromClass: aClass [
	"Safely remove the given selector from the target class.
	Be careful not to remove the selector when it has wandered
	to another package, but remove the category if it is empty."

	(aClass protocolOfSelector: aSelector) ifNotNil: [ :newProtocol |
		"If moved to and from extension, ignore removal"
		(category beginsWith: '*') = newProtocol isExtensionProtocol ifFalse: [ ^ self ].
		"Check if moved between different extension protocol"
		((category beginsWith: '*') and: [ category ~= newProtocol name ]) ifTrue: [ ^ self ] ].

	aClass
		removeSelector: aSelector;
		removeProtocolIfEmpty: category
]

{ #category : 'comparing' }
MCMethodDefinition >> requirements [
	^ { className }
]

{ #category : 'accessing' }
MCMethodDefinition >> selector [
	^selector
]

{ #category : 'comparing' }
MCMethodDefinition >> sortKey [
	^ sortKey
		ifNil: [ sortKey := self className , '.'
				,
					(self classIsMeta
						ifTrue: [ 'meta' ]
						ifFalse: [ 'nonmeta' ]) , '.' , self selector ]
]

{ #category : 'accessing' }
MCMethodDefinition >> source [
	^ source
]

{ #category : 'printing' }
MCMethodDefinition >> summary [

	^ className asString , '>>' , selector printString
]

{ #category : 'accessing' }
MCMethodDefinition >> timeStamp [
	^ timeStamp
]

{ #category : 'installing' }
MCMethodDefinition >> unload [
	"Unload me. If I represent an override method, then restore the overriden version. If not, just remove the selector from the class."

	| previousVersion |
	self isOverrideMethod ifTrue: [ previousVersion := self overridenMethodOrNil ].

	previousVersion
		ifNil: [ 
			self actualClass ifNotNil: [ :class | 
				self removeSelector: selector fromClass: class ] ]
		ifNotNil: [ previousVersion fileIn ]
]
