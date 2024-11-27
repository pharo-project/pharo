"
I add a number of facilities to basic Behaviors:
	Named instance variables
	Category organization for methods
	The notion of a name of this class (implemented as subclass responsibility)
	Logging changes on a file
	Most of the mechanism for fileOut.
	
I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.
"
Class {
	#name : 'ClassDescription',
	#superclass : 'Behavior',
	#instVars : [
		'protocols'
	],
	#category : 'Kernel-CodeModel-Classes',
	#package : 'Kernel-CodeModel',
	#tag : 'Classes'
}

{ #category : 'testing' }
ClassDescription class >> isAbstract [

	^ self == ClassDescription
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: protocol [

	<reflection: 'Class structural modification - Selector/Method modification'>
	| priorMethod priorOrigin oldProtocol newProtocol |
	self compiledMethodAt: selector ifPresent: [ :method |
		priorMethod := method.
		priorOrigin := method origin ].

	oldProtocol := self protocolOfSelector: selector.
	self codeChangeAnnouncer prevent: MethodRecategorized during: [ self classify: selector under: protocol ].
	newProtocol := self protocolOfSelector: selector.

	self addSelectorSilently: selector withMethod: compiledMethod.

	self isAnonymous ifTrue: [ ^ self ].

	"The old protocol should never be nil. But we are not in an ideal world sadly :( We got reported some cases where some code was loaded but had an error during the loading preventing a method that got added to the method dictionary to have an associated protocol. If this happens, we consider that we categorize it for the first time to avoid announcing a MethodRecategorized with an old protocol that is nil."
	(priorMethod isNil or: [ priorOrigin ~= compiledMethod origin or: [ oldProtocol isNil ] ])
		ifTrue: [
			(self packageOrganizer packageForProtocol: newProtocol from: self) addMethod: compiledMethod.
			self codeChangeAnnouncer announce: (MethodAdded method: compiledMethod) ]
		ifFalse: [ "If protocol changed and someone is from different package, I need to throw a method recategorized"
			newProtocol = oldProtocol ifFalse: [ self announceRecategorizationOf: compiledMethod oldProtocol: oldProtocol ].
			self codeChangeAnnouncer announce: (MethodModified methodChangedFrom: priorMethod to: compiledMethod oldProtocol: oldProtocol) ]
]

{ #category : 'instance variables' }
ClassDescription >> addInstVarNamed: aString [
	"Add the argument, aString, as one of the receiver's instance variables."
	<reflection: 'Class structural modification - Instance variable modification'>
	self subclassResponsibility
]

{ #category : 'protocols' }
ClassDescription >> addProtocol: aProtocol [

	| oldProtocols protocol protocolName |
	protocolName := aProtocol isString ifTrue: [ aProtocol ] ifFalse: [ aProtocol name ].

	(self hasProtocol: aProtocol) ifTrue: [ ^ self protocolNamed: protocolName ].

	protocol := Protocol named: protocolName class: self.

	oldProtocols := self protocolNames copy.

	protocols := protocols copyWith: protocol.

	"For now extensions are managed via protocols. If the protocol represent an extension we need to make sure the corresponding package exists."
	protocol isExtensionProtocol ifTrue: [ self packageOrganizer ensurePackageOfExtensionProtocol: protocol ].
	self codeChangeAnnouncer  announce: (ProtocolAdded in: self protocol: protocol).
	^ protocol
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> addSelector: selector withMethod: compiledMethod [
	<reflection: 'Class structural modification - Selector/Method modification'>
	self addAndClassifySelector: selector withMethod: compiledMethod inProtocol: nil
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> addSelectorSilently: selector withMethod: compiledMethod [

	<reflection: 'Class structural modification - Selector/Method modification'>
	super addSelectorSilently: selector withMethod: compiledMethod.
	self instanceSide noteAddedSelector: selector meta: self isMeta
]

{ #category : 'instance variables' }
ClassDescription >> addSlot: aSlot [

	<reflection: 'Class structural modification - Slot modification'>
	^self subclassResponsibility
]

{ #category : 'accessing - instances and variables' }
ClassDescription >> allInstVarNames [
	"Answer an Array of the receiver's instance variable names."
	<reflection: 'Class structural inspection - Instance variable inspection'>

	^self allSlots collect: [ :each | each name ] as: Array
]

{ #category : 'instance variables' }
ClassDescription >> allInstVarNamesEverywhere [
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^ self withAllSuperAndSubclasses flatCollectAsSet: [ :cls | cls instVarNames ]
]

{ #category : 'obsolete subclasses' }
ClassDescription >> allLocalCallsOn: aSymbol [
	"Answer all the methods that call on aSymbol, anywhere in my class hierarchy."

	^ self instanceSide withAllSuperAndSubclasses flatCollect: [ :class |
			(class thoroughWhichMethodsReferTo: aSymbol), (class class thoroughWhichMethodsReferTo: aSymbol)]
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> allSelectorsInProtocol: protocol [
	"Answer a list of all the method selectors of the receiver and all its superclasses that are in the protocol as parameter."

	^ self withAllSuperclasses flatCollectAsSet: [ :class | class selectorsInProtocol: protocol ]
]

{ #category : 'pool variable' }
ClassDescription >> allSharedPools [
	"Answer an ordered collection of the shared pools the receiver shares, including those defined  in the superclasses of the receiver."
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ OrderedCollection new
]

{ #category : 'slots' }
ClassDescription >> allSlots [
	<reflection: 'Class structural inspection - Slot inspection'>
	^self classLayout allVisibleSlots
]

{ #category : 'enumerating' }
ClassDescription >> allUnreferencedInstanceVariables [
	"Return a list of the instance variables known to the receiver which are not referenced"

	^ self allSlots reject: [:slot | slot isReferenced]
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> announceRecategorizationOf: compiledMethod oldProtocol: oldProtocol [
	"We do not want to do anything when it comes from Traits."

	compiledMethod isFromTrait ifTrue: [ ^ self ].

	self packageOrganizer repackageMethod: compiledMethod oldProtocol: oldProtocol newProtocol: compiledMethod protocol.
	self codeChangeAnnouncer announce: (MethodRecategorized method: compiledMethod oldProtocol: oldProtocol)
]

{ #category : 'authors' }
ClassDescription >> authors [
	"Returns a bag representing the author frequency based on the latest version of the methods of the receiver."

	^(self methods, self classSide methods) collect: [ :each | each author ] as: Bag
]

{ #category : 'accessing - comment' }
ClassDescription >> classCommentBlank [
	"Classes can override this method to show another template."

	"There are two use cases: a class hierarchy can give information about what kind of comment is
	useful (see 'PackageManifest class'). If in addition, '#hasComment ^true' can be implemented in
	cases where the class does not need a dedicated comment. See 'TestCase class'for an example"

	| stream |
	stream := (String new: 100) writeStream.
	stream nextPutAll: 'Please comment me using the following template inspired by Class Responsibility Collaborator (CRC) design:

For the Class part:  State a one line summary. For example, "I represent a paragraph of text".

For the Responsibility part: Three sentences about my main responsibilities - what I do, what I know.

For the Collaborators Part: State my main collaborators and one line about how I interact with them.

Public API and Key Messages

- message one
- message two
- (for bonus points) how to create instances.

   One simple example is simply gorgeous.

Internal Representation and Key Implementation Points.'.
	self instanceVariables ifNotEmpty: [
		stream
			cr;
			cr;
			nextPutAll: '    Instance Variables' ].

	self instVarNames sorted do: [ :each |
		stream
			cr;
			tab;
			nextPutAll: each;
			nextPut: $:;
			tab;
			tab;
			nextPutAll: '<Object>' ].
	stream cr.
	stream
		cr;
		cr;
		nextPutAll: '    Implementation Points'.
	^ stream contents
]

{ #category : 'accessing - parallel hierarchy' }
ClassDescription >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	"Point classSide >>> Point class"
	"Point class classSide >>> Point class"
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^ self subclassResponsibility
]

{ #category : 'instance variables' }
ClassDescription >> classThatDefinesInstVarNamed: instVarName [
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^self
		slotNamed: instVarName
		 ifFound: [ :slot | slot definingClass  ]
		 ifNone: nil
]

{ #category : 'printing' }
ClassDescription >> classVariablesOn: aStream [
	"Write my class variable names separated by spaces on the argument."

	self classVarNames
		do: [ :each | aStream nextPutAll: each ]
		separatedBy: [ aStream space ]
]

{ #category : 'printing' }
ClassDescription >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^ String streamContents: [ :stream | self classVariablesOn: stream ]
]

{ #category : 'protocols' }
ClassDescription >> classify: selector under: aProtocol [

	| oldProtocol newProtocol |
	oldProtocol := self protocolOfSelector: selector.

	"In case the method is already classified and we say to classify it under nil, we do not update the protocol."
	(aProtocol isNil and: [ oldProtocol isNotNil ]) ifTrue: [ ^ self ].
	
	"If we try to classify the method as an extension of its own package, just unclassify the method to avoid any weird state where the method could be considered as an extension or not depending on the interpretation."
	(self isProtocolExtensionFromTheSamePackage: aProtocol) ifTrue: [
		ExtensionPointsOwningPackageNotification signalFor: self package name.
		^ self classify: selector under: Protocol unclassified ].

	(newProtocol := self ensureProtocol: aProtocol) = oldProtocol ifTrue: [ ^ self ].

	oldProtocol ifNotNil: [
		oldProtocol removeMethodSelector: selector.
		self removeProtocolIfEmpty: oldProtocol ].

	newProtocol addMethodSelector: selector.

	"During the first classification of a method we dont need to announce the classification because users can subscribe to the method added announcement."
	oldProtocol ifNotNil: [ self notifyOfRecategorizedSelector: selector from: oldProtocol to: newProtocol ]
]

{ #category : 'compiling' }
ClassDescription >> compile: sourceCode classified: protocol [
	"Compile the source code in the context of the receiver and install the result in the receiver's method dictionary under the given protocol (this can be a protocol or a protocol name).
	
	The argument sourceCode is either a string or an object that converts to a string or a PositionableStream on an object that converts to a string."

	^ self compile: sourceCode classified: protocol notifying: nil
]

{ #category : 'compiling' }
ClassDescription >> compile: sourceCode classified: protcol notifying: requestor [

	^ self
		  compile: sourceCode
		  classified: protcol
		  withStamp: nil
		  notifying: requestor
]

{ #category : 'compiling' }
ClassDescription >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor [

	^ self
		  compile: sourceCode
		  classified: protocol
		  withStamp: changeStamp
		  notifying: requestor
		  logSource: true
]

{ #category : 'compiling' }
ClassDescription >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor logSource: logSource [

	| method |

	method := self compiler
		source: sourceCode;
		requestor: requestor;
		failBlock: (requestor ifNotNil: [ [ ^ nil ] ]); "no failblock if no requestor"
		permitUndeclared: (requestor isNil); "compatibility: permit undeclared if no requestor"
		protocol: protocol;
		changeStamp: changeStamp;
		logged: logSource;
		install.

	^ method selector
]

{ #category : 'compiling' }
ClassDescription >> compile: sourceCode notifying: requestor [
	"Refer to the comment in Behavior|compile:notifying:."

	^ self compile: sourceCode classified: nil notifying: requestor
]

{ #category : 'compiling' }
ClassDescription >> compileSilently: sourceCode [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: sourceCode classified: 'not defined protocol' notifying: nil
]

{ #category : 'compiling' }
ClassDescription >> compileSilently: sourceCode classified: protocolName [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: sourceCode classified: protocolName notifying: nil
]

{ #category : 'compiling' }
ClassDescription >> compileSilently: sourceCode classified: protocolName notifying: requestor [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list."

	^ self codeChangeAnnouncer suspendAllWhile: [ self compile: sourceCode classified: protocolName notifying: requestor ]
]

{ #category : 'copying' }
ClassDescription >> copyAllMethodsFrom: originClass [
	"Install a copy of each methods of originClass in myself under the same protocol."

	originClass methods do: [ :aMethod |
		(self includesLocalSelector: aMethod selector) ifTrue: [
			aMethod sourceCode asString = (self sourceCodeAt: aMethod selector) asString ifFalse: [
				self error: ('{1}>>{2} will be redefined if you proceed.' format: {
							 self name.
							 aMethod selector }) ] ].

		self compile: aMethod sourceCode classified: aMethod protocol ]
]

{ #category : 'slots' }
ClassDescription >> definesSlot: aSlot [
	"Return true whether the receiver defines an instance variable named aString"
	<reflection: 'Class structural inspection - Slot inspection'>
	^ self slots identityIncludes: aSlot
]

{ #category : 'slots' }
ClassDescription >> definesSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	<reflection: 'Class structural inspection - Slot inspection'>

	^ self slotNames includes: aString
]

{ #category : 'dependencies' }
ClassDescription >> dependentClasses [
	"Return the list of classes used myself"

	| classes |
	classes := Set new.

	"A class depends on its superclass"
	self superclass ifNotNil: [ :class | classes add: class ].

	"We unify a class and its metaclass"
	self methods , self classSide methods do: [ :method |
		method literalsDo: [ :literal | "We also check if the method is not an extension"
			(literal isVariableBinding and: [ literal isGlobalClassNameBinding and: [ method isExtension not ] ]) ifTrue: [ classes add: literal value ] ] ].

	^ classes asArray
]

{ #category : 'accessing' }
ClassDescription >> deprecatedAliases [
	"I return a potential list of alias names for myself that are deprecated."

	^ self propertyAt: #deprecatedAliases ifAbsent: [ {  } ]
]

{ #category : 'accessing' }
ClassDescription >> deprecatedAliases: aCollection [
	"I allow one to declare deperecated names for myself. Typically, in case I get renamed, I can be used to declare the old name as a deprecated name.
	Compared to the use of #isDeprecated, I have some advantages such as:
	- Not needing to maintain a deprecated class
	- Keep #isKindOf: working
	- Keep #on:do: working
	- And more cases like the previous two.
	
	In the future this should be improved so that we can declare the deprecated aliases in the FluidClassBuilder. But it requires a way to persist this information in Tonel.
	Maybe we should also do another improvement that is to remove from the system dectionary the aliases if we set some aliases while I already had some set before."

	| environment |
	environment := self class environment.
	
	"If a global already has the name of an alias then we should warn the user."
	aCollection do: [ :alias |
		environment at: alias asSymbol ifPresent: [ :class |
			"If the global exists but points to the same class then it means that we are just reaaplying the aliases."
			class ~= self ifTrue: [
				self notify: alias , ' is been applied as a deprecated alias for ' , self class name , ' but a global already exists under this name.' ] ] ].

	self propertyAt: #deprecatedAliases put: (aCollection collect: [ :name | name asSymbol ]).
	self deprecatedAliases do: [ :deprecatedName |
		environment at: deprecatedName put: self.
		(environment lookupVar: deprecatedName) isDeprecated: true ]
]

{ #category : 'queries - protocols' }
ClassDescription >> ensureProtocol: aProtocol [
	"I can take a Protocol or a protocol name as paramater.
	
	If my parameter is a name, I'll return a protocol associated with it. A new one if needed.
	If my parameter is a Protocol, I'll return it if it comes from me or I'll create one of the same name."

	aProtocol ifNil: [ ^ self ensureProtocol: Protocol unclassified ].

	^ self addProtocol: aProtocol
]

{ #category : 'accessing - packages' }
ClassDescription >> extendingPackages [
	"the extending packages of a class are the packages that extend it."

	^ (self localMethods , self classSide localMethods
		   select: [ :each | each isExtension ]
		   thenCollect: [ :each | each extensionPackage ]) asIdentitySet
]

{ #category : 'protocols' }
ClassDescription >> extensionProtocols [

	^ self protocols select: [ :protocol | protocol isExtensionProtocol ]
]

{ #category : 'accessing - packages' }
ClassDescription >> extensionSelectors [
	^ self extendingPackages flatCollect: [ :each | each extensionSelectorsForClass: self ]
]

{ #category : 'instance variables' }
ClassDescription >> forceNewFrom: anArray [
    "Create a new instance of the class and fill
    its instance variables up with the array."
    | object max |

    object := self new.
    max := self instSize.
    anArray doWithIndex: [:each :index |
        index > max ifFalse:
            [object instVarAt: index put: each]].
    ^ object
]

{ #category : 'accessing - parallel hierarchy' }
ClassDescription >> hasClassSide [
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^self subclassResponsibility
]

{ #category : 'instance variables' }
ClassDescription >> hasInstVarNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^ self instVarNames includes: aString
]

{ #category : 'protocols' }
ClassDescription >> hasProtocol: aProtocol [

	^ self protocolNames includes: (aProtocol isString ifTrue: [ aProtocol ] ifFalse: [ aProtocol name ])
]

{ #category : 'pool variable' }
ClassDescription >> hasSharedPools [
	"Only a class may have shared pools"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ false
]

{ #category : 'slots' }
ClassDescription >> hasSlot: aSlot [
	"Return true whether the receivers hierarchy defines an instance variable named aString."

	^ self allSlots identityIncludes: aSlot
]

{ #category : 'slots' }
ClassDescription >> hasSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString.
	this includes non-visible slots"
	<reflection: 'Class structural inspection - Slot inspection'>
	^ self classLayout hasSlotNamed: aString
]

{ #category : 'pool variable' }
ClassDescription >> includesSharedPoolNamed:  aSharedPoolString [
	"Answer whether the receiver uses the shared pool named aSharedPoolString"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ self sharedPools anySatisfy: [:each | each name = aSharedPoolString]
]

{ #category : 'initialization' }
ClassDescription >> initialize [

	super initialize.
	self resetProtocols
]

{ #category : 'instance variables' }
ClassDescription >> instVarIndexFor: instVarName [
	"Answer the index of the named instance variable."
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^self instVarIndexFor: instVarName ifAbsent: 0
]

{ #category : 'instance variables' }
ClassDescription >> instVarIndexFor: instVarName ifAbsent: aBlock [
	"Answer the index of the named instance variable."
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^self
		slotNamed: instVarName
		ifFound: [ :slot | slot isVirtual ifTrue: [aBlock value] ifFalse: [slot index]]
		ifNone: aBlock
]

{ #category : 'private' }
ClassDescription >> instVarMappingFrom: oldClass [
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames := oldClass allInstVarNames.
	^self allInstVarNames
			collect: [:instVarName | oldInstVarNames indexOf: instVarName]
]

{ #category : 'instance variables' }
ClassDescription >> instVarNames [
	"Answer an Array of the receiver's instance variable names."
	<reflection: 'Class structural inspection - Instance variable inspection'>
	^self slots collect: [ :each | each name ]
]

{ #category : 'accessing - parallel hierarchy' }
ClassDescription >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	"Point instanceSide >>> Point"
	"Point class instanceSide >>> Point"
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^ self subclassResponsibility
]

{ #category : 'printing' }
ClassDescription >> instanceVariablesOn: aStream [
	"Write my instance variable names separated by spaces on the argument."

	self localSlots
		do: [ :each | aStream nextPutAll: each name asString]
		separatedBy: [ aStream space ]
]

{ #category : 'printing' }
ClassDescription >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^ String streamContents: [ :stream | self instanceVariablesOn: stream ]
]

{ #category : 'accessing - parallel hierarchy' }
ClassDescription >> isClassSide [
	"Return true whether the receiver is a metaclass (in a couple class/metaclass sense)."
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	"Point isClassSide >>> false"
	"Point class isClassSide >>> true"

	^self == self classSide
]

{ #category : 'testing' }
ClassDescription >> isDeprecated [
	^ self package isDeprecated
]

{ #category : 'testing' }
ClassDescription >> isExtended [

	^ self extendingPackages isEmpty
]

{ #category : 'testing' }
ClassDescription >> isExtendedInPackage: aPackage [
	"returns true if aPackage defines an extension to this class"
	^ aPackage extendsClass: self
]

{ #category : 'testing' }
ClassDescription >> isInstalled [
	"Return true if I am installed in the system (which is equivalent to me been part of my package organizer). Else, return false."

	^ self packageOrganizer isClassInstalled: self
]

{ #category : 'accessing - parallel hierarchy' }
ClassDescription >> isInstanceSide [
	"Return true whether the receiver is a class (in a couple class/metaclass sense)."
	"Point isInstanceSide >>> true"
	"Point class isInstanceSide >>> false"
	<reflection: 'Class structural inspection - Class/Metaclass shift'>
	^ self isClassSide not
]

{ #category : 'testing' }
ClassDescription >> isLocalSelector: aSelector [

	<reflection: 'Class structural inspection - Selectors and methods inspection'>
	^ self methodDict includesKey: aSelector
]

{ #category : 'accessing - deprecated parallel hierarchy' }
ClassDescription >> isMeta [
	<reflection: 'Class structural inspection - Class kind testing'>
	^self isClassSide
]

{ #category : 'private' }
ClassDescription >> isProtocolExtensionFromTheSamePackage: aProtocol [
	"I return true if the protocol as parameter is an extension protocol and the concerned package is my origin package."

	| protocolName |
	aProtocol ifNil: [ ^ false ].

	protocolName := aProtocol isString
		                ifTrue: [ aProtocol ]
		                ifFalse: [ aProtocol name ].

	"If it is not an extension then there is no problem"
	protocolName first = $* ifFalse: [ ^ false ].

	^ (self packageOrganizer packageMatchingExtensionName: protocolName allButFirst) = self package
]

{ #category : 'testing' }
ClassDescription >> isTaggedWith: aSymbol [

	^ self packageTagName
		  ifNil: [ false ]
		  ifNotNil: [ :tag | tag = aSymbol ]
]

{ #category : 'private' }
ClassDescription >> linesOfCode [
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."

	| lines |
	lines := self localMethods inject: 0 into: [ :sum :each | sum + each linesOfCode ].
	^ self isMeta
		ifTrue: [ lines ]
		ifFalse: [ lines + self classSide linesOfCode ]
]

{ #category : 'slots' }
ClassDescription >> localSlots [
	<reflection: 'Class structural inspection - Slot inspection'>

	^ self slots select: [ :aSlot | aSlot isDefinedByOwningClass ]
]

{ #category : 'protocols' }
ClassDescription >> methodsInProtocol: protocol [

	^ (self selectorsInProtocol: protocol) collect: [ :selector | self compiledMethodAt: selector ]
]

{ #category : 'slots' }
ClassDescription >> needsSlotClassDefinition [
    "return true if we define something else than InstanceVariableSlots or normal class variables"

    ^ self slotsNeedFullDefinition or: [ self class slotsNeedFullDefinition ]
]

{ #category : 'private' }
ClassDescription >> newInstanceFrom: oldInstance variable: variable size: instSize [
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"

	| new  value |
	variable
		ifTrue: [ new := self basicNew: oldInstance basicSize ]
		ifFalse: [ new := self basicNew ].

	"Slot migration happens there"
	self allSlots
		do: [ :newSlot |
			oldInstance class
				slotNamed: newSlot name
				ifFound: [ :oldSlot |
					newSlot wantsInitialization ifTrue: [ self initializeSlots: new ].
					value := oldSlot read: oldInstance.
					newSlot write: value to: new ] ].
	variable
		ifTrue: [ 1 to: oldInstance basicSize do: [ :offset | new basicAt: offset put: (oldInstance basicAt: offset) ] ].
	^ new
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> noteAddedSelector: aSelector meta: isMeta [
	"A hook allowing some classes to react to adding of certain selectors"
]

{ #category : 'compiling' }
ClassDescription >> noteCompilationOf: aSelector meta: isMeta [
	"A hook allowing some classes to react to recompilation of certain selectors"
]

{ #category : 'organization updating' }
ClassDescription >> notifyOfRecategorizedSelector: selector from: oldProtocol to: newProtocol [
	"If compiled method is not there, it meens it has been removed, not recategorized... so I skip
	 the method recategorized announce"

	self compiledMethodAt: selector ifPresent: [ :method | self announceRecategorizationOf: method oldProtocol: oldProtocol ]
]

{ #category : 'private' }
ClassDescription >> numberOfMethods [
	"count all methods that are local (not comming from a trait)"
	| num |
	num := self localMethods size.
	^ self isMeta
		ifTrue: [ num ]
		ifFalse: [ num + self classSide numberOfMethods ]
]

{ #category : 'initialization' }
ClassDescription >> obsolete [
	"Make the receiver obsolete."
	self superclass removeSubclass: self.
	super obsolete
]

{ #category : 'accessing' }
ClassDescription >> package [

	^ self subclassResponsibility
]

{ #category : 'accessing - packages' }
ClassDescription >> package: aPackage [

	^ (self packageOrganizer ensurePackage: aPackage) addClass: self
]

{ #category : 'accessing - packages' }
ClassDescription >> package: aPackage tag: aTag [

	^ (self packageOrganizer ensureTag: aTag inPackage: aPackage) addClass: self
]

{ #category : 'accessing - packages' }
ClassDescription >> packageName [

	^ self package ifNotNil: [ :package | package name ]
]

{ #category : 'accessing' }
ClassDescription >> packageTag [
	"Package tags are sub categories of packages to have a better organization of the packages."

	^ self subclassResponsibility
]

{ #category : 'accessing - packages' }
ClassDescription >> packageTag: aTag [
	(aTag isString ifTrue: [ self package ensureTag: aTag ] ifFalse: [ aTag ]) addClass: self
]

{ #category : 'accessing - packages' }
ClassDescription >> packageTagName [
	"Package tags are sub categories of packages to have a better organization of the packages. I return the name of my package tag.."

	^ self packageTag name
]

{ #category : 'accessing - packages' }
ClassDescription >> packages [
	"the extending packages of a class are the packages that extend it."

	^ self extendingPackages
		  add: self package;
		  yourself
]

{ #category : 'printing' }
ClassDescription >> printOn: aStream [
	aStream nextPutAll: self name
]

{ #category : 'protocols' }
ClassDescription >> protocolNameOfSelector: aSelector [
	"Return the protocol name including the method of the same name as the selector.
	If the class does not includes a method of this name, returns nil. Maybe this should be changed for an error in the future."

	^ (self protocolOfSelector: aSelector) ifNotNil: [ :protocol | protocol name ]
]

{ #category : 'protocols' }
ClassDescription >> protocolNamed: aString [

	^ self protocolNamed: aString ifAbsent: [ NotFound signalFor: aString ]
]

{ #category : 'protocols' }
ClassDescription >> protocolNamed: aString ifAbsent: aBlock [

	^ self protocols
		  detect: [ :e | e name = aString ]
		  ifNone: aBlock
]

{ #category : 'protocols' }
ClassDescription >> protocolNames [
	"Return the list of all the protocol names included in this class."

	^ self protocols collect: [ :protocol | protocol name ]
]

{ #category : 'protocols' }
ClassDescription >> protocolOfSelector: aSelector [
	"Return the protocol including the method of the same name as the selector.
	If the class does not includes a method of this name, returns nil. Maybe this should be changed for an error in the future."

	^ self protocols
		  detect: [ :each | each includesSelector: aSelector ]
		  ifNone: [ nil ]
]

{ #category : 'protocols' }
ClassDescription >> protocols [
	"I return all the protocols contained in me.
	In the past I was returning the protocol names but now I am returning the instances directly. If you want to deal with the names you can use #protocolNames."

	^ protocols
]

{ #category : 'protocols' }
ClassDescription >> protocols: aCollection [
	
	protocols := aCollection
]

{ #category : 'compiling' }
ClassDescription >> reformatAll [
	"Reformat all methods in this class"

	self methods do: [ :method | method reformat ]
]

{ #category : 'protocols' }
ClassDescription >> removeEmptyProtocols [
	"We copy protocols because it is usually bad to remove elements of a collection while iterating on it"

	self protocols copy do: [ :protocol | self removeProtocolIfEmpty: protocol ]
]

{ #category : 'protocols' }
ClassDescription >> removeFromProtocols: aSelector [

	(self protocolOfSelector: aSelector) ifNotNil: [ :protocol |
		protocol removeMethodSelector: aSelector.
		self removeProtocolIfEmpty: protocol ]
]

{ #category : 'instance variables' }
ClassDescription >> removeInstVarNamed: aString [
	"Remove the argument, aString, as one of the receiver's instance variables."
	<reflection: 'Class structural modification - Instance variable modification'>
	^self removeSlot: (self slotNamed: aString)
]

{ #category : 'protocols' }
ClassDescription >> removeNonexistentSelectorsFromProtocols [
	"For each protocol, remove the selectors that are not present in the class."

	self protocols do: [ :protocol |
		protocol methodSelectors
			reject: [ :selector | self includesSelector: selector ]
			thenDo: [ :selector | self removeFromProtocols: selector ] ]
]

{ #category : 'accessing - packages' }
ClassDescription >> removePackageTag [
	"Removing a package tag is the same as moving it to the root tag that is the uncategorized tag."

	| package |
	package := self package.
	package moveClass: self toTag: package rootTag
]

{ #category : 'protocols' }
ClassDescription >> removeProtocol: aProtocol [
	"Remove all methods present in the given protocol (or protocol name) and remove the protocol.
	Does nothing if the protocol does not exists."

	| protocol |
	(self hasProtocol: aProtocol) ifFalse: [ ^ self ].
	protocol := self ensureProtocol: aProtocol.
	protocol methodSelectors do: [ :sel | self removeSelector: sel ].
	self removeProtocolIfEmpty: protocol 
]

{ #category : 'protocols' }
ClassDescription >> removeProtocolIfEmpty: aProtocol [
	"I take a protocol or a protocol name and remvoe it if it is empty."

	| protocol oldProtocolNames |
	(self hasProtocol: aProtocol) ifFalse: [ ^ self ].

	protocol := self ensureProtocol: aProtocol.

	protocol isEmpty ifFalse: [ ^ self ].

	oldProtocolNames := self protocolNames.
	protocols := protocols copyWithout: protocol.
	self codeChangeAnnouncer  announce: (ProtocolRemoved in: self protocol: protocol)
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> removeSelector: selector [
	"Remove the message whose selector is given from the method
	dictionary of the receiver, if it is there. Answer nil otherwise."

	<reflection: 'Class structural modification - Selector/Method modification'>
	| method origin |
	method := self compiledMethodAt: selector ifAbsent: [ ^ nil ].
	origin := method origin.

	"Save the protocol in the properties so that method can still answer their protocol after been removed."
	method propertyAt: #protocol put: (self protocolOfSelector: selector).
	method removeFromPackage.
	self removeFromProtocols: selector.

	super removeSelector: selector.

	self codeChangeAnnouncer methodRemoved: method origin: origin
]

{ #category : 'instance variables' }
ClassDescription >> removeSlot: aSlot [
	<reflection: 'Class structural modification - Slot modification'>
	^self subclassResponsibility
]

{ #category : 'protocols' }
ClassDescription >> renameProtocol: anOldProtocol as: aNewProtocol [

	| oldProtocol newProtocol |
	(aNewProtocol isNil or: [ anOldProtocol isNil ]) ifTrue: [ ^ self ].

	(self hasProtocol: anOldProtocol) ifFalse: [ ^ self ].
	
	"We should not be able to have a protocol that is an extension of the same class so let's skip the rename."
	(self isProtocolExtensionFromTheSamePackage: aNewProtocol) ifTrue: [ 
		ExtensionPointsOwningPackageNotification signalFor: self package name.
		^ self ].

		oldProtocol := self ensureProtocol: anOldProtocol.
		newProtocol := self ensureProtocol: aNewProtocol.

		oldProtocol = newProtocol ifTrue: [ ^ self ].

		newProtocol addAllMethodsFrom: oldProtocol.
		oldProtocol resetMethodSelectors.
		self removeProtocolIfEmpty: oldProtocol.

	"I need to notify also the selector changes, otherwise Package will not notice"
	newProtocol methodSelectors do: [ :each | self notifyOfRecategorizedSelector: each from: oldProtocol to: newProtocol ]
]

{ #category : 'organization' }
ClassDescription >> reorganize [
	"During fileIn, !Rectangle reorganize! allows Rectangle to seize control and treat the next chunk as its organization.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"

	^ self
]

{ #category : 'protocols' }
ClassDescription >> resetProtocols [

	protocols := Array new
]

{ #category : 'protocols' }
ClassDescription >> selectorsInProtocol: aProtocol [
	"Answer a list of the selectors of the receiver that are in specific protocol (or protocol name)"

	(self hasProtocol: aProtocol) ifFalse: [ ^ #(  ) ].

	^ (self ensureProtocol: aProtocol) methodSelectors
]

{ #category : 'pool variable' }
ClassDescription >> sharedPoolOfVarNamed: aString [
	"Only classes may have shared pools"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ nil
]

{ #category : 'printing' }
ClassDescription >> sharedPoolString [
	"Answer a string of my shared pools separated by spaces."

	^String streamContents: [ :stream |
		self sharedPoolStringOn: stream ]
]

{ #category : 'printing' }
ClassDescription >> sharedPoolStringOn: aStream [
	"Write my shared pools separated by dots on argument, aStream."

	self sharedPools do:  [ :p | aStream nextPutAll: p name ] separatedBy: [ aStream nextPutAll: ' . ' ]
]

{ #category : 'pool variable' }
ClassDescription >> sharedPools [
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ OrderedCollection new
]

{ #category : 'printing' }
ClassDescription >> sharedPoolsOn: aStream [
	"Answer a string of my shared pool names separated by spaces."

	self sharedPools
		do: [ :each |
				aStream nextPutAll: (self environment
					keyAtIdentityValue: each
					ifAbsent: [
						each isObsolete
							ifTrue: [ each name ] "obsolete classes should be visible"
							ifFalse: [ 'private' ] ]) ] "if class is from different environment, mark it as private"
			separatedBy: [ aStream space ]
]

{ #category : 'pool variable' }
ClassDescription >> sharedPoolsString [
	"Answer a string of my shared pool names separated by spaces."

	^String streamContents: [ :stream |
		self sharedPools
			do: [ :each |
				stream nextPutAll: (self environment
					keyAtIdentityValue: each
					ifAbsent: [
						each isObsolete
							ifTrue: [ each name ] "obsolete classes should be visible"
							ifFalse: [ 'private' ] ]) ] "if class is from different environment, mark it as private"
			separatedBy: [ stream space ] ]
]

{ #category : 'slots' }
ClassDescription >> slotNamed: aName [
	<reflection: 'Class structural inspection - Slot inspection'>
	^self classLayout resolveSlot: aName asSymbol
]

{ #category : 'slots' }
ClassDescription >> slotNamed: aName ifFound: foundBlock [
	<reflection: 'Class structural inspection - Slot inspection'>
	^self slotNamed: aName ifFound: foundBlock ifNone: [ "do nothing" ]
]

{ #category : 'slots' }
ClassDescription >> slotNamed: aName ifFound: foundBlock ifNone: exceptionBlock [
	<reflection: 'Class structural inspection - Slot inspection'>
	^self classLayout resolveSlot: aName ifFound: foundBlock ifNone: exceptionBlock
]

{ #category : 'slots' }
ClassDescription >> slotNames [
	<reflection: 'Class structural inspection - Slot inspection'>
	^self slots collect: [ :each | each name ]
]

{ #category : 'accessing' }
ClassDescription >> slots [
	<reflection: 'Class structural inspection - Slot inspection'>
	^self classLayout visibleSlots
]

{ #category : 'slots' }
ClassDescription >> slotsNeedFullDefinition [
	"return true if we define something else than InstanceVariableSlots"
	^self slots anySatisfy: [ :each | each needsFullDefinition ]
]

{ #category : 'private' }
ClassDescription >> spaceUsed [
	^super spaceUsed + (self hasClassSide
		ifTrue: [self classSide spaceUsed]
		ifFalse: [0])
]

{ #category : 'storing' }
ClassDescription >> storeOn: aStream [
	"Classes and Metaclasses have global names."

	aStream nextPutAll: self name
]

{ #category : 'initialization' }
ClassDescription >> superclass: aSuperclass layout: aLayout [
	layout := aLayout.

	self
		superclass: aSuperclass
		methodDictionary: self emptyMethodDictionary
		format: aLayout format
]

{ #category : 'initialization' }
ClassDescription >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"

	super superclass: aClass methodDictionary: mDict format: fmt.
	self resetProtocols
]

{ #category : 'initialization' }
ClassDescription >> superclass: aSuperclass withLayoutType: layoutType slots: slotArray [
	| superLayout newScope newLayout |
	superLayout := aSuperclass
						ifNil: [ EmptyLayout instance ]
						ifNotNil: [ aSuperclass classLayout ].
	newScope := superLayout slotScope extend: slotArray.
	newLayout := layoutType extending: superLayout scope: newScope host: self.
	newLayout checkSanity.
	self
		superclass: aSuperclass
		layout: newLayout
]

{ #category : 'accessing - method dictionary' }
ClassDescription >> uncategorizedSelectors [

	^ self selectorsInProtocol: Protocol unclassified
]

{ #category : 'pool variable' }
ClassDescription >> usesLocalPoolVarNamed: aString [
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^false
]

{ #category : 'pool variable' }
ClassDescription >> usesPoolVarNamed: aString [
	"Only classes may use a pool variable named: aString"
	<reflection: 'Class structural inspection - Shared pool inspection'>
	^ false
]

{ #category : 'queries' }
ClassDescription >> whichMethodsAccess: instVarName [
	"Answer the methods that access the argument, instVarName, as a named instance variable. Pay attention the instVarName variable should be used e.g., be part of a return, message send or assignment else the compiler optimize it away and the method will not return it."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self methods select: [ :m | slot isAccessedIn: m ] ]
		ifNone: [ #() ]
]

{ #category : 'queries' }
ClassDescription >> whichMethodsRead: instVarName [
	"Answer the selectors whose methods read the argument, instVarName, as a named instance variable.
	Pay attention the instVarName variable should be used e.g., be part of a return, message send or assignment else the compiler optimize it away and the method will not return it."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self methods select: [ :m | slot isReadIn: m ] ]
		ifNone: [ #() ]
]

{ #category : 'queries' }
ClassDescription >> whichMethodsWrite: instVarName [
	"Answer the selectors whose methods write the argument, instVarName, as a named instance variable.
	Pay attention the instVarName variable should be used e.g., be part of a return, message send or assignment else the compiler optimize it away and the method will not return it."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self methods select: [ :m | slot isWrittenIn: m ] ]
		ifNone: [ #() ]
]

{ #category : 'queries' }
ClassDescription >> whichSelectorsAccess: instVarName [
	"Answer the selectors whose methods access the argument, instVarName, as a named instance variable. Pay attention the instVarName variable should be used e.g., be part of a return, message send or assignment else the compiler optimize it away and the method will not return it."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self selectors select: [ :sel | slot isAccessedIn: self>>sel ] ]
		ifNone: [ #() ]
]

{ #category : 'queries' }
ClassDescription >> whichSelectorsRead: instVarName [
	"Answer the selectors whose methods read the argument, instVarName, as a named instance variable.
	Pay attention the instVarName variable should be used e.g., be part of a return, message send or assignment else the compiler optimize it away and the method will not return it."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self selectors select: [ :sel | slot isReadIn: self >> sel ] ]
		ifNone: [ #() ]
]

{ #category : 'queries' }
ClassDescription >> whichSelectorsWrite: instVarName [
	"Answer the selectors whose methods write the argument, instVarName, as a named instance variable.
	Pay attention the instVarName variable should be used e.g., be part of a return, message send or assignment else the compiler optimize it away and the method will not return it."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self selectors select: [ :sel | slot isWrittenIn: self >> sel ] ]
		ifNone: [ #() ]
]
