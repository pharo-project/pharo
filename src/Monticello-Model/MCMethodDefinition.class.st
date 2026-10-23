"
A MCMethodDefinition represents a method definition. 
It captures the following information.

## Implementation details

It might be long to get the timestamp of a method since it requires to access the source file. As an optimization we might save the source pointer instead and retireve the timestamp lazily in case we don't always need this timestamp.

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
		'sortKey',
		'sourcePointer'
	],
	#classVars : [
		'InitializersEnabled'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'instance creation' }
MCMethodDefinition class >> className: classString classIsMeta: metaBoolean selector: selectorString category: protocolName sourcePointer: aNumber source: sourceString [

	^ self new
		  className: classString;
		  selector: selectorString;
		  protocol: protocolName;
		  sourcePointer: aNumber;
		  classIsMeta: metaBoolean;
		  source: sourceString;
		  yourself
]

{ #category : 'instance creation' }
MCMethodDefinition class >> className: classString classIsMeta: metaBoolean selector: selectorString category: protocolName timeStamp: timeString source: sourceString [

	^ self new
		  className: classString;
		  selector: selectorString;
		  protocol: protocolName;
		  timeStamp: timeString;
		  classIsMeta: metaBoolean;
		  source: sourceString;
		  yourself
]

{ #category : 'instance creation' }
MCMethodDefinition class >> className: classString selector: selectorString category: catString timeStamp: timeString source: sourceString [

	^ self
		  className: classString
		  classIsMeta: false
		  selector: selectorString
		  category: catString
		  timeStamp: timeString
		  source: sourceString
]

{ #category : 'settings' }
MCMethodDefinition class >> initializersEnabled [

	^ InitializersEnabled ifNil: [true]
]

{ #category : 'settings' }
MCMethodDefinition class >> initializersEnabled: aBoolean [

	InitializersEnabled := aBoolean
]

{ #category : 'comparing' }
MCMethodDefinition >> = aDefinition [
	^ super = aDefinition
		and: [ aDefinition protocol = self protocol
		and: [ aDefinition source withInternalLineEndings = self source withInternalLineEndings ] ]
]

{ #category : 'accessing' }
MCMethodDefinition >> actualClass [

	^ self actualClassIn: self class environment
]

{ #category : 'accessing' }
MCMethodDefinition >> actualClassIn: anEnvironment [

	anEnvironment ifNil: [ ^ self actualClass ].
	
	^ anEnvironment at: className ifPresent: [ :class |
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
MCMethodDefinition >> classIsMeta: aBoolean [

	classIsMeta := aBoolean
]

{ #category : 'accessing' }
MCMethodDefinition >> className [
	^className
]

{ #category : 'serializing' }
MCMethodDefinition >> className: aString [

	className := aString asSymbol
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
	hash := String stringHash: classIsMeta printString initialHash: 0.
	hash := String stringHash: source initialHash: hash.
	hash := String stringHash: category initialHash: hash.
	hash := String stringHash: className initialHash: hash.
	^ hash
]

{ #category : 'testing' }
MCMethodDefinition >> isAlreadyDefined [

	^ self actualClass compiledMethodAt: self selector ifPresent: [ true ] ifAbsent: [ false ]
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
MCMethodDefinition >> loadWithLoader: aLoader [

	| compiler method actualClass methodInstaller |
	actualClass := self actualClassIn: aLoader targetEnvironment.
	actualClass ifNil: [ Error signal: 'Class not found: ' , className printString ].

	compiler := actualClass compiler
		            permitFaulty: aLoader isStrict not;
		            permitUndeclared: aLoader isStrict not;
		            source: source.

	method := compiler compileOrFail.
	methodInstaller := (actualClass methodInstaller
			   logged: true;
			   changeStamp: timeStamp;
			   protocol: category).
	methodInstaller install: method into: actualClass compiler: compiler.

	selector := method selector
]

{ #category : 'installing' }
MCMethodDefinition >> postloadOver: aDefinition [

	super postloadOver: aDefinition.
	self class initializersEnabled ifTrue: [
			(self isInitializer and: [ self actualClass isTrait not and: [ aDefinition isNil or: [ self source ~= aDefinition source ] ] ]) ifTrue: [
				self actualClass instanceSide initialize ] ].
		
	"Postloading of FFI fields. This code will be called when loading FFI structures that are not by default in the image. This is NOT dead code."
	self class environment at: #ExternalStructure ifPresent: [
		self isExternalStructureFieldDefinition ifTrue: [ self actualClass instanceSide compileFields ] ]
]

{ #category : 'accessing' }
MCMethodDefinition >> protocol [
	"Return in which protocol (conceptual groups of methods) the receiver is grouped into."
	^ category
]

{ #category : 'serializing' }
MCMethodDefinition >> protocol: anObject [

	category := anObject isEmptyOrNil
		            ifTrue: [ Protocol unclassified ]
		            ifFalse: [ anObject asSymbol ]
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

{ #category : 'serializing' }
MCMethodDefinition >> selector: aString [

	selector := aString asSymbol
]

{ #category : 'comparing' }
MCMethodDefinition >> sortKey [

	^ sortKey ifNil: [
			  sortKey := Symbol streamContents: [ :aStream |
					             aStream
						             nextPutAll: self className;
						             nextPut: $.;
						             nextPutAll: (self classIsMeta
								              ifTrue: [ 'meta' ]
								              ifFalse: [ 'nonmeta' ]);
						             nextPut: $.;
						             nextPutAll: self selector ] ]
]

{ #category : 'accessing' }
MCMethodDefinition >> source [
	^ source
]

{ #category : 'serializing' }
MCMethodDefinition >> source: aString [

	source := aString withInternalLineEndings
]

{ #category : 'accessing' }
MCMethodDefinition >> sourcePointer: anObject [
	sourcePointer := anObject
]

{ #category : 'printing' }
MCMethodDefinition >> summary [

	^ className asString , '>>' , selector printString
]

{ #category : 'accessing' }
MCMethodDefinition >> timeStamp [
	"My computation is lazy because it takes time to compute and is not always necessary."

	^ timeStamp ifNil: [ sourcePointer ifNotNil: [ :pointer | timeStamp := SourcesManager current timeStampAt: pointer ] ]
]

{ #category : 'serializing' }
MCMethodDefinition >> timeStamp: aString [

	timeStamp := aString
]

{ #category : 'installing' }
MCMethodDefinition >> unload [

	self actualClass ifNotNil: [ :class | self removeSelector: selector fromClass: class ]
]
