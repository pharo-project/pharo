"
Specific version of compiled code for methods. They encode only the method's bytecodes and literals, blocks have each a CompiledBlock.

In addition the execution mechanics, a compiled method has two extra literals. The last literal is the class in which the method is installed. The last but one literal is either the method's selector or an AdditionalMethodState instance. AdditionalMethodState instances are used to encode pragmas and properties.

The bytecode encodes the trailer at the end, see #trailerSize for the current size used.
Thus endPC of a compiled method is not the size of the bytecode, but the size minus the fixed length of the trailer.

"
Class {
	#name : 'CompiledMethod',
	#superclass : 'CompiledCode',
	#type : 'compiledMethod',
	#category : 'Kernel-CodeModel-Methods',
	#package : 'Kernel-CodeModel',
	#tag : 'Methods'
}

{ #category : 'constants' }
CompiledMethod class >> abstractMarker [
	^ #subclassResponsibility
]

{ #category : 'class initialization' }
CompiledMethod class >> checkBytecodeSetConflictsInMethodsWith: aBlock [

	self allSubInstances
		detect: aBlock
		ifFound: [ Warning signal: 'There are existing CompiledMethods with a different encoderClass.' ]
]

{ #category : 'class initialization' }
CompiledMethod class >> checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass [
	(aBytecodeEncoderSubclass inheritsFrom: BytecodeEncoder) ifFalse:
		[self error: 'A bytecode set encoder is expected to be a subclass of BytecodeEncoder']
]

{ #category : 'constants' }
CompiledMethod class >> conflictMarker [
	^ #traitConflict
]

{ #category : 'constants' }
CompiledMethod class >> disabledMarker [
	^ #shouldNotImplement
]

{ #category : 'constants' }
CompiledMethod class >> explicitRequirementMarker [
	^ #explicitRequirement
]

{ #category : 'class initialization' }
CompiledMethod class >> fullFrameSize [  "CompiledMethod fullFrameSize"
	^ LargeFrame
]

{ #category : 'instance creation' }
CompiledMethod class >> headerFlagForEncoder: anEncoderClass [
	anEncoderClass == PrimaryBytecodeSetEncoderClass ifTrue: [ ^ 0 ].
	anEncoderClass == SecondaryBytecodeSetEncoderClass ifTrue: [ ^ SmallInteger minVal ].
	self error: 'The encoder is not one of the two installed bytecode sets'
]

{ #category : 'class initialization' }
CompiledMethod class >> initialize [    "CompiledMethod initialize"
	"Initialize class variables specifying the size of the temporary frame
	needed to run instances of me."

	SmallFrame := 16.	"Context range for temps+stack"
	LargeFrame := 56.
	PrimaryBytecodeSetEncoderClass := nil. "Old encoder was removed"
	SecondaryBytecodeSetEncoderClass := EncoderForSistaV1
]

{ #category : 'class initialization' }
CompiledMethod class >> installPrimaryBytecodeSet: aBytecodeEncoderSubclass [
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[ ^self ].
	self checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass.
	self checkBytecodeSetConflictsInMethodsWith: [:m|
		m usesPrimaryBytecodeSet and: [m encoderClass ~~ aBytecodeEncoderSubclass]].
	PrimaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass
]

{ #category : 'class initialization' }
CompiledMethod class >> installSecondaryBytecodeSet: aBytecodeEncoderSubclass [
	PrimaryBytecodeSetEncoderClass == aBytecodeEncoderSubclass ifTrue:
		[ ^ self ].
	self checkIsValidBytecodeEncoder: aBytecodeEncoderSubclass.
	self checkBytecodeSetConflictsInMethodsWith: [ :m |
		m usesSecondaryBytecodeSet and: [ m encoderClass ~~ aBytecodeEncoderSubclass ] ].
	SecondaryBytecodeSetEncoderClass := aBytecodeEncoderSubclass
]

{ #category : 'accessing - class hierarchy' }
CompiledMethod class >> methodPropertiesClass [
	"Answer the class to use to create a method's properties, which can be a poor man's way
	 to add instance variables to subclassses of CompiledMethod.  Subclasses of CompiledMethod
	 should define a corresponding subclass of AdditionalMethodState that adds any instance variables
	 required, and override this method to answer that class."
	^AdditionalMethodState
]

{ #category : 'instance creation' }
CompiledMethod class >> new [
	"This will not make a meaningful method, but it could be used
	to invoke some otherwise useful method in this class."
	^self basicNew: 2 header: 1024
]

{ #category : 'class initialization' }
CompiledMethod class >> smallFrameSize [

	^ SmallFrame
]

{ #category : 'constants' }
CompiledMethod class >> subclassResponsibilityMarker [
	^ #subclassResponsibility
]

{ #category : 'constants' }
CompiledMethod class >> trailerSize [
	"we use the last 5 byte to store the source pointer"
	^ 5
]

{ #category : 'queries' }
CompiledMethod >> allIgnoredNotImplementedSelectors [

	^ (self pragmaAt: #ignoreNotImplementedSelectors:)
		  ifNotNil: [ :pragma | pragma argumentAt: 1 ]
		  ifNil: [ #(  ) ]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> cachePragmas [

	self pragmas do: [ :pragma | pragma class addToCache: pragma ]
]

{ #category : 'accessing' }
CompiledMethod >> classBinding [
	"answer the association to the class that I am installed in, or nil if none."
	^self literalAt: self numLiterals
]

{ #category : 'accessing' }
CompiledMethod >> classBinding: aBinding [
       "sets the association to the class that I am installed in"
       ^self literalAt: self numLiterals put: aBinding
]

{ #category : 'source code management' }
CompiledMethod >> clearSourcePointer [
	"we use #setSourcePointer: to not clear the property #source"
	self setSourcePointer: 0
]

{ #category : 'accessing' }
CompiledMethod >> codeForNoSource [
	"if everything fails, decompile the bytecode"
	"If there is no compiler, we cannot decompile it"
	Smalltalk hasCompiler ifFalse: [ ^ nil ].

	 ^(self compiler decompileMethod: self) formattedCode
]

{ #category : 'testing' }
CompiledMethod >> containsHalt [

	^ self hasProperty: #containsHalt
]

{ #category : 'accessing' }
CompiledMethod >> defaultSelector [
	"Invent and answer an appropriate message selector (a Symbol) for me,
	that is, one that will parse with the correct number of arguments."

	^ #DoIt numArgs: self numArgs
]

{ #category : 'displaying' }
CompiledMethod >> displayStringOn: aStream [
	aStream print: self methodClass; nextPutAll: '>>'; store: self selector
]

{ #category : 'accessing' }
CompiledMethod >> endPC [
	"Answer the index of the last bytecode"
	^ self size - self class trailerSize
]

{ #category : 'accessing' }
CompiledMethod >> flushCache [
	"Tell the virtual machine to remove all references to this method from its method lookup caches, and to discard any optimized version 	of the method, if it has any of these.  This must be done whenever a method is modified in place, such as modifying its literals or 	machine code, to reflect the revised code.  c.f. Behavior>>flushCache & Symbol>>flushCache.  Essential.	 See MethodDictionary class 	comment."

	<primitive: 116>
]

{ #category : 'source code management' }
CompiledMethod >> getPreambleFrom: aFileStream at: position [
	^ SourceFiles getPreambleFrom: aFileStream at: position
]

{ #category : 'source code management' }
CompiledMethod >> getSourceFromFile [
	"PLEASE Note: clients should always call #sourceCode"
	"Read the source code from file, determining source file index and
	file position from the last bytes of this method (size defined in #trailerSize)."

	^ [ SourceFiles sourceCodeAt: self sourcePointer ] on: Error do: [ nil ]
]

{ #category : 'source code management' }
CompiledMethod >> getSourceReplacingSelectorWith: newSelector [
	| oldKeywords newKeywords source newSource oldSelector start |
	source := self sourceCode.

	source ifNil: [ ^ nil ].

	oldSelector := self selector.
	oldSelector = newSelector ifTrue: [ ^ source ].

	oldKeywords := oldSelector keywords.
	newKeywords := (newSelector ifNil: [self defaultSelector]) keywords.
	[oldKeywords size = newKeywords size] assert.

	newSource := source.
	start := 1.
	oldKeywords with: newKeywords do: [:oldKey :newKey| |pos|
		pos := newSource findString: oldKey startingAt: start .
		newSource := newSource copyReplaceFrom: pos to: (pos + oldKey size -1) with: newKey.
		start := pos + newKey size ].

	^newSource
]

{ #category : 'testing' }
CompiledMethod >> hasComment [
	"check if this method has a method comment"
	^self comment isEmptyOrNil not
]

{ #category : 'testing' }
CompiledMethod >> hasNonLocalReturn [
	"Answer whether any block has a method-return ('^') in its code."
	self allBlocksDo: [ :each | each hasMethodReturn ifTrue: [ ^true ] ].
	^false
]

{ #category : 'testing' }
CompiledMethod >> hasPragma [
	<reflection: 'Class structural inspection - Pragma'>
	^ self hasProperties and: [self pragmas isNotEmpty]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> hasPragmaNamed: aSymbol [
	<reflection: 'Class structural inspection - Pragma'>
	^ self pragmas anySatisfy: [ :pragma | pragma selector = aSymbol ]
]

{ #category : 'testing' }
CompiledMethod >> hasProperties [
	^ self penultimateLiteral isMethodProperties
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> hasProperty: aKey [

	| extendedMethodState |
	^ (extendedMethodState := self penultimateLiteral) isMethodProperties
		  and: [  extendedMethodState includesProperty: aKey ]
]

{ #category : 'accessing' }
CompiledMethod >> hasSourceCode [

	^ self sourceCodeOrNil isNotNil
]

{ #category : 'testing' }
CompiledMethod >> hasSourcePointer [
	^ self sourcePointer ~= 0
]

{ #category : 'testing' }
CompiledMethod >> isAbstract [
	"Answer true if I am abstract"

	| marker |
	marker := self markerOrNil ifNil: [^false].

	^marker == self class subclassResponsibilityMarker
		or: [ marker == self class explicitRequirementMarker ]
]

{ #category : 'testing' }
CompiledMethod >> isBinarySelector [
	^self selector
		allSatisfy: [:each | each isSpecial]
]

{ #category : 'testing' }
CompiledMethod >> isClassSide [
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^self methodClass isClassSide
]

{ #category : 'testing' }
CompiledMethod >> isClassified [

	^ self protocol isUnclassifiedProtocol not
]

{ #category : 'testing' }
CompiledMethod >> isCompiledMethod [

	^ true
]

{ #category : 'testing' }
CompiledMethod >> isConflict [
	^ self markerOrNil == self class conflictMarker
]

{ #category : 'testing' }
CompiledMethod >> isDeprecated [

	^ self sendsAnySelectorOf: #(
			#deprecated:
			#deprecated:transformWith:
			#deprecated:transformWith:when:
			#deprecated:on:in:
			#deprecated:on:in:transformWith:
			#deprecated:on:in:transformWith:when:)
]

{ #category : 'testing' }
CompiledMethod >> isDisabled [
	^ self isDisabled: self markerOrNil
]

{ #category : 'testing' }
CompiledMethod >> isDisabled: marker [
	^ marker == self class disabledMarker
]

{ #category : 'testing' }
CompiledMethod >> isDoIt [
	^self selector isDoIt
]

{ #category : 'testing' }
CompiledMethod >> isExplicitlyRequired [
	^ self isExplicitlyRequired: self markerOrNil
]

{ #category : 'testing' }
CompiledMethod >> isExplicitlyRequired: marker [
	^ marker == self class explicitRequirementMarker
]

{ #category : 'testing' }
CompiledMethod >> isExtension [
	"I return true if a method is an extension method. Which means that the methods is not packaged in the package of the class containing the method, but in another package."

	"(self >> #selector) isExtension >>> false"

	"(self >> #traitSource) isExtension >>> true"

	^ self hasProperty: #extensionPackage
]

{ #category : 'testing' }
CompiledMethod >> isFaulty [
 	"check if this method was compiled from syntactically wrong code"
	| ast |
	"fast pre-check: all methods with syntax errors reference this symbol"
  	(self refersToLiteral: #signalSyntaxError:) ifFalse: [ ^false].

	"Check the ast in cache, if any"
	ast := self propertyAt: #ast ifAbsent: [].

	ast ifNil: [
		"Warning, the following does not work for doit methods because this assumes `noPrefix:false` "
		ast := self methodClass compiler
				source: self sourceCode;
				parse ].
	^ ast isFaulty
]

{ #category : 'testing' }
CompiledMethod >> isFromTrait [
	"Return true for methods that have been included from Traits"

	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	^ self hasProperty: #traitSource
]

{ #category : 'testing' }
CompiledMethod >> isInstalled [
	self methodClass ifNotNil:
		[:class|
		self selector ifNotNil:
			[:selector|
			^self == (class compiledMethodAt: selector ifAbsent: nil)]].
	^false
]

{ #category : 'testing' }
CompiledMethod >> isInstanceSide [
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^self methodClass isInstanceSide
]

{ #category : 'testing' }
CompiledMethod >> isOverridden [
	| selector|
	selector := self selector.
	self methodClass allSubclassesDo: [:each |
		(each includesSelector: selector)
			ifTrue: [ ^ true ]].
	^ false
]

{ #category : 'testing' }
CompiledMethod >> isProvided [
	^ self isProvided: self markerOrNil
]

{ #category : 'testing' }
CompiledMethod >> isProvided: marker [
	marker ifNil: [^ true].
	^ (self isRequired: marker) not and: [(self isDisabled: marker) not]
]

{ #category : 'testing' }
CompiledMethod >> isRequired [
	"A method is required if it is a method declaring a subclass responsibility or an explicit requirement. This mean the method needs to be overriden (in case of subclass reponsibility) or implemented (in case of explicit requirement)."

	^ self isRequired: self markerOrNil
]

{ #category : 'testing' }
CompiledMethod >> isRequired: marker [
	marker ifNil: [^ false].
	(self isExplicitlyRequired: marker) ifTrue: [^ true].
	(self isSubclassResponsibility: marker) ifTrue: [^ true].
	^ false
]

{ #category : 'testing' }
CompiledMethod >> isReturnField [
	"Answer whether the receiver is a quick return of an instance variable."
	^ self primitive between: 264 and: 519
]

{ #category : 'printing' }
CompiledMethod >> isSelfEvaluating [

	^self methodClass isNotNil and: [self isDoIt not]
]

{ #category : 'testing' }
CompiledMethod >> isSubclassResponsibility [
	^ self isSubclassResponsibility: self markerOrNil
]

{ #category : 'testing' }
CompiledMethod >> isSubclassResponsibility: marker [
	^ marker == self class subclassResponsibilityMarker
]

{ #category : 'source code management' }
CompiledMethod >> linesOfCode [
	"An approximate measure of lines of code.
	Includes method's name and comments, but excludes empty lines."
	| lines |
	lines := 0.
	self sourceCode lineIndicesDo: [:start :endWithoutDelimiters :end |
		endWithoutDelimiters > start ifTrue: [lines := lines+1]].
	^lines
]

{ #category : 'literals' }
CompiledMethod >> literalsToSkip [

	^ 2
]

{ #category : 'private' }
CompiledMethod >> markerOrNil [
	^ self encoderClass markerOrNilFor: self
]

{ #category : 'accessing' }
CompiledMethod >> method [
	"polymorphic with closure"

	^ self
]

{ #category : 'accessing' }
CompiledMethod >> methodClass [
	"answer the class that I am installed in"
	^self classBinding value
]

{ #category : 'accessing' }
CompiledMethod >> methodClass: aClass [
	"set the class binding in the last literal to aClass"
	^self numLiterals > 0
		ifTrue: [ self literalAt: self numLiterals put: aClass binding ]
]

{ #category : 'private' }
CompiledMethod >> migratePersistingPropertiesIn: aNewCompileMethod [
	"While creating a new version of a method, we might want to persist some properties. I am here for this. I migrate the properties to persist from myself, the old version of the method, to the new version."

	| properties |
	(properties := self penultimateLiteral) isMethodProperties ifFalse: [ ^ self "There is no property so nothing to migrate." ].

	properties class propertiesToPersist do: [ :propertyName |
		properties at: propertyName ifPresent: [ :value | aNewCompileMethod propertyAt: propertyName put: value ] ]
]

{ #category : 'accessing' }
CompiledMethod >> name [
	^ self printString
]

{ #category : 'accessing' }
CompiledMethod >> overriddenMethod [

	^ self methodClass superclass ifNotNil: [ :class | class lookupSelector: self selector ]
]

{ #category : 'accessing' }
CompiledMethod >> pathOfLiteralIndexes [
	
	^ #[]
]

{ #category : 'private' }
CompiledMethod >> penultimateLiteral [
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	^(pIndex := self numLiterals - 1) > 0
		ifTrue: [self literalAt: pIndex]
		ifFalse: [nil]
]

{ #category : 'private' }
CompiledMethod >> penultimateLiteral: anObject [
	"Answer the penultimate literal of the receiver, which holds either
	 the receiver's selector or its properties (which will hold the selector)."
	| pIndex |
	(pIndex := self numLiterals - 1) > 0
		ifTrue: [self literalAt: pIndex put: anObject]
		ifFalse: [self error: 'insufficient literals']
]

{ #category : 'copying' }
CompiledMethod >> postCopy [
	| penultimateLiteral |
	(penultimateLiteral := self penultimateLiteral) isMethodProperties ifTrue:
		[self penultimateLiteral: (penultimateLiteral copy
									method: self;
									yourself).
		 self penultimateLiteral pragmas do:
			[:p| p method: self]]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> pragmaAt: aKey [
	"Answer the first pragma with selector aKey, or nil if none."
	<reflection: 'Class structural inspection - Pragma'>
	^ self pragmas detect: [ :pragma | pragma selector == aKey ] ifNone: nil
]

{ #category : 'literals' }
CompiledMethod >> pragmaRefersToLiteral: literal [

	^ self penultimateLiteral isMethodProperties and: [
		  self pragmas anySatisfy: [ :pragma | pragma refersToLiteral: literal ] ]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> pragmas [
	| selectorOrProperties |
	<reflection: 'Class structural inspection - Pragma'>
	^(selectorOrProperties := self penultimateLiteral) isMethodProperties
		ifTrue: [selectorOrProperties pragmas]
		ifFalse: [#()]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> pragmasDo: aBlock [
	<reflection: 'Class structural inspection - Pragma'>
	| selectorOrProperties |
	(selectorOrProperties := self penultimateLiteral) isMethodProperties
		ifTrue: [selectorOrProperties pragmasDo: aBlock]
]

{ #category : 'debugger support' }
CompiledMethod >> prepareForSimulationWith: numArgs [
	"This method changes the argument count of a CompiledMethod header to numArgs, its temporary count to numArgs + 1 and change the code handling primitive error to store the error code in the unique temporary of the method"

	| newHeader |
	newHeader := (((self header bitAnd: 2r01110000000000111111111111111111)
			bitOr: (numArgs bitShift: 24))
			bitOr: (numArgs + 1 bitShift: 18)).
	newHeader := newHeader + (self class headerFlagForEncoder: self encoderClass).
	self objectAt: 1 put: newHeader.

	self encoderClass prepareMethod: self forSimulationWith: numArgs
]

{ #category : 'printing' }
CompiledMethod >> printOn: aStream [
	"Overrides method inherited from the byte arrayed collection."

	aStream print: self methodClass; nextPutAll: '>>'; print: self selector
]

{ #category : 'printing' }
CompiledMethod >> printPrimitiveOn: aStream [
	"Print the primitive on aStream"
	| primDecl |
	self isPrimitive ifFalse: [ ^self ].

	self isExternalCallPrimitive ifTrue:
		[^aStream print: (self literalAt: 1); cr].
	aStream nextPutAll: '<primitive: '.
	self isNamedPrimitive
		ifTrue:
			[primDecl := self literalAt: 1.
			 (primDecl at: 2) asString printOn: aStream.
			 (primDecl at: 1) ifNotNil:
				[:moduleName|
				aStream nextPutAll:' module: '.
				moduleName asString printOn: aStream]]
		ifFalse:
			[aStream print: self primitive].
	self primitiveErrorVariableName ifNotNil:
		[:primitiveErrorVariableName|
		 aStream nextPutAll: ' error: '; nextPutAll: primitiveErrorVariableName].
	aStream nextPut: $>; cr
]

{ #category : 'accessing' }
CompiledMethod >> properties [
	"Answer the method properties of the receiver."
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector]
		ifFalse: [self class methodPropertiesClass forMethod: self selector: propertiesOrSelector]
]

{ #category : 'accessing' }
CompiledMethod >> properties: aMethodProperties [
	"Set the method-properties of the receiver to aMethodProperties."

	self
		literalAt: self numLiterals - 1
		put:
			(aMethodProperties
				ifEmpty: [ aMethodProperties selector ]
				ifNotEmpty: [
			 		aMethodProperties
				 		setMethod: self;
				 		yourself ])
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> propertyAt: propName [
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifAbsent: [nil]]
		ifFalse: [nil]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> propertyAt: propName ifAbsent: aBlock [
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifAbsent: aBlock]
		ifFalse: [aBlock value]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> propertyAt: aKey ifAbsentPut: aBlock [
	"Answer the property associated with aKey or, if aKey isn't found store the result of evaluating aBlock as new value."

	^ self propertyAt: aKey ifAbsent: [ self propertyAt: aKey put: aBlock value ]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> propertyAt: propName ifPresent: aBlock [
	| propertiesOrSelector |
	^(propertiesOrSelector := self penultimateLiteral) isMethodProperties
		ifTrue: [propertiesOrSelector propertyAt: propName ifPresent: aBlock]
		ifFalse: [nil]
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> propertyAt: propName put: propValue [
	"Set or add the property with key propName and value propValue.
	 If the receiver does not yet have a method properties create one and replace
	 the selector with it.  Otherwise, either relace propValue in the method properties
	 or replace method properties with one containing the new property."
	| propertiesOrSelector |
	(propertiesOrSelector := self penultimateLiteral) isMethodProperties ifFalse:
		[self penultimateLiteral: ((self class methodPropertiesClass
									selector: propertiesOrSelector
									with: (Association
											key: propName asSymbol
											value: propValue))
									setMethod: self;
									yourself).
		^propValue].
	(propertiesOrSelector includesProperty: propName) ifTrue:
		[^propertiesOrSelector at: propName put: propValue].
	self penultimateLiteral: (propertiesOrSelector
								copyWith: (Association
												key: propName asSymbol
												value: propValue)).
	^propValue
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> propertyKeysAndValuesDo: aBlock [
	"Enumerate the receiver with all the keys and values."

	| propertiesOrSelector |
	(propertiesOrSelector := self penultimateLiteral) isMethodProperties ifTrue:
		[propertiesOrSelector propertyKeysAndValuesDo: aBlock]
]

{ #category : 'protocols' }
CompiledMethod >> protocol [
	"Return in which protocol (conceptual groups of methods) the receiver is grouped into.
	
	With the current implementation the method will have to lookup the protocol in its class, but the #protocol property will be added when we remove a method to still be able to answer the protocol the method was into."

	^ self propertyAt: #protocol ifAbsent: [ self methodClass protocolOfSelector: self selector ]
]

{ #category : 'protocols' }
CompiledMethod >> protocol: aProtocol [
	^ self methodClass classify: self selector under: aProtocol
]

{ #category : 'protocols' }
CompiledMethod >> protocolName [
	"Return the name of the protocol (conceptual groups of methods) in which the receiver is grouped into."

	^ self protocol ifNotNil: [ :protocol | protocol name ]
]

{ #category : 'scanning' }
CompiledMethod >> readsField: varIndex [
	"Answer whether the receiver loads the instance variable indexed by the argument."
	self isReturnField ifTrue: [^self returnField = (varIndex - 1)].
	^ super readsField: varIndex
]

{ #category : 'evaluating' }
CompiledMethod >> receiver: aReceiver withArguments: anArray executeMethod: aMethod [
	"execute aMethod.
	This method takes aMethod as an argument so we can call primitive 188.
	Clients should use #valueWithReceiver:arguments:
	
	We have this method here in addition to ProtoObject>>#withArgs:executeMethod:
	so we can execute a method without sending a message to aReceiver"
		<reflection: 'Message sending and code execution - Arbitrary method/primitive execution'>
	<primitive: 188>
	self primitiveFailed
]

{ #category : 'literals' }
CompiledMethod >> refersToLiteral: aLiteral [
	"Answer true if any literal in this method is literal, even if embedded in array structure."

	^(self pragmaRefersToLiteral: aLiteral) or: [ super refersToLiteral: aLiteral]
]

{ #category : 'private' }
CompiledMethod >> removeFromPackage [

	self isFromTrait ifFalse: [ self package ifNotNil: [ :package | package removeMethod: self ] ]
]

{ #category : 'initialization' }
CompiledMethod >> removeFromSystem [
	^ self methodClass removeSelector: self selector
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> removeProperty: propName [
	"Remove the property propName if it exists.
	 Do _not_ raise an error if the property is missing."
	| value |
	value := self propertyAt: propName ifAbsent: [^nil].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).

	"Remove unneded AdditionalMethodState"
	self penultimateLiteral size = 0 ifTrue: [ self penultimateLiteral: self selector ].
	^value
]

{ #category : 'accessing - pragmas & properties' }
CompiledMethod >> removeProperty: propName ifAbsent: aBlock [
	"Remove the property propName if it exists.
	 Answer the evaluation of aBlock if the property is missing."
	| value |
	value := self propertyAt: propName ifAbsent: [^aBlock value].
	self penultimateLiteral: (self penultimateLiteral copyWithout:
									(Association
										key: propName
										value: value)).
	^value
]

{ #category : 'accessing' }
CompiledMethod >> returnField [
	"Answer the index of the instance variable returned by a quick return
	method."
	| prim |
	prim := self primitive.
	prim < 264
		ifTrue: [self error: 'only meaningful for quick-return']
		ifFalse: [^ prim - 264]
]

{ #category : 'accessing' }
CompiledMethod >> selector [
	"Answer a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	| penultimateLiteral |
	^(penultimateLiteral := self penultimateLiteral) isMethodProperties
		ifTrue: [penultimateLiteral selector]
		ifFalse: [penultimateLiteral]
]

{ #category : 'accessing' }
CompiledMethod >> selector: aSelector [
	"Set a method's selector.  This is either the penultimate literal,
	 or, if the method has any properties or pragmas, the selector of
	 the MethodProperties stored in the penultimate literal."
	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	| penultimateLiteral nl |
	(penultimateLiteral := self penultimateLiteral) isMethodProperties
		ifTrue: [penultimateLiteral selector: aSelector]
		ifFalse: [(nl := self numLiterals) < 2 ifTrue:
					[self error: 'insufficient literals to hold selector'].
				self literalAt: nl - 1 put: aSelector]
]

{ #category : 'source code management' }
CompiledMethod >> setSourcePointer: srcPointer [

	| trailerBytes trailerSize start |
	trailerSize := self class trailerSize.
	trailerBytes := srcPointer asByteArrayOfSize: trailerSize.
	start := self size - trailerSize.
	start + 1 to: self size do: [:n | self at: n put: (trailerBytes at: n - start) ]
]

{ #category : 'accessing' }
CompiledMethod >> sourceCode [
	"Retrieve or reconstruct the source code for this method."

	^ self sourceCodeOrNil
		  ifNotNil: [ :code | ^ code ]
		  ifNil: [ self codeForNoSource ]
]

{ #category : 'accessing' }
CompiledMethod >> sourceCodeOrNil [
	"Retrieve the source code for this method, or return nil."

	self propertyAt: #source ifPresent:  [ :code | ^ code ].
	self hasSourcePointer ifFalse: [ ^ nil ].
	^ self getSourceFromFile
]

{ #category : 'source code management' }
CompiledMethod >> sourcePointer [
	"Answer the integer which can be used to find the source file and position for this method.
	The actual interpretation of this number is up to the SourceFileArray stored in the global variable SourceFiles."
	| trailerBytes trailerSize start |
	trailerSize := self class trailerSize.
	trailerBytes := ByteArray new: trailerSize.
	start := self size - trailerSize.
	start + 1 to: self size do: [:n | trailerBytes at: n - start put: (self at: n) ].
	^ trailerBytes asInteger
]

{ #category : 'source code management' }
CompiledMethod >> sourcePointer: srcPointer [

	"Drop the #source property if any"
	self removeProperty: #source.
	self setSourcePointer: srcPointer
]

{ #category : 'debugger support' }
CompiledMethod >> stepIntoQuickMethods: aBoolean [
	^self propertyAt: #stepIntoQuickMethod put: aBoolean
]

{ #category : 'storing' }
CompiledMethod >> storeOn: aStream [
	| noneYet |
	aStream nextPutAll: '(('.
	aStream nextPutAll: self class name.
	aStream nextPutAll: ' newMethod: '.
	aStream store: self size - self initialPC + 1.
	aStream nextPutAll: ' header: '.
	aStream store: self header.
	aStream nextPut: $).
	noneYet := self storeElementsFrom: self initialPC to: self endPC on: aStream.
	1 to: self numLiterals do:
		[:index |
		noneYet
			ifTrue: [noneYet := false]
			ifFalse: [aStream nextPut: $;].
		aStream nextPutAll: ' literalAt: '.
		aStream store: index.
		aStream nextPutAll: ' put: '.
		aStream store: (self literalAt: index)].
	noneYet ifFalse: [aStream nextPutAll: '; yourself'].
	aStream nextPut: $)
]

{ #category : 'source code management' }
CompiledMethod >> tempNames [
	"on the level of the compiled method, tempNames includes argument names"
	^self ast argumentNames, self ast temporaryNames
]

{ #category : 'protocols' }
CompiledMethod >> unclassify [

	self protocol: Protocol unclassified
]

{ #category : 'testing' }
CompiledMethod >> usesUndeclareds [
	"this is mixing Undeclareds and Obsoletes, maybe we should have two methods"
	self withAllNestedLiteralsDo: [ :literal |
		literal class == UndeclaredVariable ifTrue: [ ^ true ].
		(literal isBlock not and: [
			 literal value isBehavior and: [ literal value isObsolete ] ])
			ifTrue: [ ^ true ] ].
	^ false
]

{ #category : 'evaluating' }
CompiledMethod >> valueWithReceiver: aReceiver [
	<reflection: 'Message sending and code execution - Arbitrary method/primitive execution'>
	^self receiver: aReceiver withArguments: #() executeMethod: self
]

{ #category : 'evaluating' }
CompiledMethod >> valueWithReceiver: aReceiver arguments: anArray [
	<reflection: 'Message sending and code execution - Arbitrary method/primitive execution'>
	^self receiver: aReceiver withArguments: anArray executeMethod: self
]

{ #category : 'copying' }
CompiledMethod >> veryDeepCopyWith: deepCopier [
	"Return self.  I am always shared.  Do not record me.  Only use this for blocks.  Normally methodDictionaries should not be copied this way."
]

{ #category : 'scanning' }
CompiledMethod >> writesField: varIndex [
	"Answer whether the receiver stores into the instance variable indexed by the argument."
	self isQuick ifTrue: [^false].
	^ super writesField: varIndex
]
