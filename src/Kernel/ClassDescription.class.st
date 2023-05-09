"
I add a number of facilities to basic Behaviors:
	Named instance variables
	Category organization for methods
	The notion of a name of this class (implemented as subclass responsibility)
	The maintenance of a ChangeSet, and logging changes on a file
	Most of the mechanism for fileOut.
	
I am an abstract class, in particular, my facilities are intended for inheritance by two subclasses, Class and Metaclass.
"
Class {
	#name : #ClassDescription,
	#superclass : #Behavior,
	#instVars : [
		'organization'
	],
	#category : #'Kernel-Classes'
}

{ #category : #compiling }
ClassDescription >> acceptsLoggingOfCompilation [
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself
	Weird name is so that it will come lexically before #compile, so that a clean build can make it through."

	^ true
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: protocol [

	| priorMethod priorOrigin oldProtocol newProtocol |
	self compiledMethodAt: selector ifPresent: [ :method |
		priorMethod := method.
		priorOrigin := method origin ].

	oldProtocol := self organization protocolOfSelector: selector.
	SystemAnnouncer uniqueInstance suspendAllWhile: [ "The next part is really ugly with the double if.. BUT! I will fix that in a new PR soon... I want to remove the usage of `Protocol unclassified` as a way to say 'Do not upatde the protocol' because this is counter intuitive."
		self organization classify: selector under: ((protocol isString
				  ifTrue: [ protocol ]
				  ifFalse: [ protocol name ]) = Protocol unclassified
				 ifTrue: [ oldProtocol ]
				 ifFalse: [ protocol ]) ].
	newProtocol := self organization protocolOfSelector: selector.

	self addSelectorSilently: selector withMethod: compiledMethod.

	self isAnonymous ifTrue: [ ^ self ].

	(priorMethod isNil or: [ priorOrigin ~= compiledMethod origin ])
		ifTrue: [ SystemAnnouncer uniqueInstance methodAdded: compiledMethod ]
		ifFalse: [ "If protocol changed and someone is from different package, I need to throw a method recategorized"
			self
				notifyRepackage: selector
				method: compiledMethod
				oldProtocol: oldProtocol
				newProtocol: newProtocol.

			SystemAnnouncer uniqueInstance methodChangedFrom: priorMethod to: compiledMethod oldProtocol: oldProtocol name ]
]

{ #category : #'instance variables' }
ClassDescription >> addInstVarNamed: aString [
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility
]

{ #category : #'accessing - tags' }
ClassDescription >> addMethodTag: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose.
	In class we could define what tags could be used for methods"

	self organization addProtocol: aSymbol
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> addSelector: selector withMethod: compiledMethod [

	| priorMethodOrNil oldProtocol |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: [ nil ].
	oldProtocol := self organization protocolOfSelector: selector.

	"If there is no old protocol, then we want to ensure the method is at least in Protocol unclassified to not have protocolless methods."
	oldProtocol ifNil: [ SystemAnnouncer uniqueInstance suspendAllWhile: [ self organization classify: selector under: nil ] ].

	self addSelectorSilently: selector withMethod: compiledMethod.
	priorMethodOrNil
		ifNil: [ SystemAnnouncer uniqueInstance methodAdded: compiledMethod ]
		ifNotNil: [ SystemAnnouncer uniqueInstance methodChangedFrom: priorMethodOrNil to: compiledMethod oldProtocol: (oldProtocol ifNotNil: [ oldProtocol name ]) ]
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> addSelectorSilently: selector withMethod: compiledMethod [
	super addSelectorSilently: selector withMethod: compiledMethod.
	self instanceSide noteAddedSelector: selector meta: self isMeta
]

{ #category : #'instance variables' }
ClassDescription >> addSlot: aSlot [

	^self subclassResponsibility
]

{ #category : #'accessing - instances and variables' }
ClassDescription >> allInstVarNames [
	"Answer an Array of the receiver's instance variable names."

	^self allSlots collect: [ :each | each name ] as: Array
]

{ #category : #'instance variables' }
ClassDescription >> allInstVarNamesEverywhere [
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	^ self withAllSuperAndSubclasses flatCollectAsSet: [ :cls | cls instVarNames ]
]

{ #category : #'obsolete subclasses' }
ClassDescription >> allLocalCallsOn: aSymbol [
	"Answer all the methods that call on aSymbol, anywhere in my class hierarchy."

	^ self instanceSide withAllSuperAndSubclasses flatCollect: [ :class |
			(class thoroughWhichMethodsReferTo: aSymbol), (class class thoroughWhichMethodsReferTo: aSymbol)]
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> allSelectorsInProtocol: aName [
	"Answer a list of all the methods of the receiver and all its
	superclasses that are in the protocol named aName"

	^ self withAllSuperclasses flatCollectAsSet: [ :class | class organization methodSelectorsInProtocol: aName ]
]

{ #category : #'pool variable' }
ClassDescription >> allSharedPools [
	"Answer an ordered collection of the shared pools the receiver shares, including those defined  in the superclasses of the receiver."

	^ OrderedCollection new
]

{ #category : #slots }
ClassDescription >> allSlots [
	^self classLayout allVisibleSlots
]

{ #category : #enumerating }
ClassDescription >> allUnreferencedInstanceVariables [
	"Return a list of the instance variables known to the receiver which are not referenced"

	^ self allSlots reject: [:slot | slot isReferenced]
]

{ #category : #authors }
ClassDescription >> authors [
	"Returns a bag representing the author frequency based on the latest version of the methods of the receiver."

	^(self methods, self classSide methods) collect: [ :each | each author ] as: Bag
]

{ #category : #'accessing - deprecated parallel hierarchy' }
ClassDescription >> baseClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #instanceSide instead' transformWith:  '`@receiver baseClass'
						-> '`@receiver instanceSide'.
	^ self instanceSide
]

{ #category : #'accessing - comment' }
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

{ #category : #'accessing - parallel hierarchy' }
ClassDescription >> classSide [
	"Return the metaclass of the couple class/metaclass. Useful to avoid explicit test."
	"Point classSide >>> Point class"
	"Point class classSide >>> Point class"

	^ self subclassResponsibility
]

{ #category : #'instance variables' }
ClassDescription >> classThatDefinesClassVariable: classVarName [
	"Answer the class that defines the given class variable"

	(self classPool includesKey: classVarName asSymbol) ifTrue: [^ self].
	^self superclass ifNotNil: [self superclass classThatDefinesClassVariable: classVarName]
]

{ #category : #'instance variables' }
ClassDescription >> classThatDefinesInstVarNamed: instVarName [
	^self
		slotNamed: instVarName
		 ifFound: [ :slot | slot definingClass  ]
		 ifNone: nil
]

{ #category : #printing }
ClassDescription >> classVariablesOn: aStream [
	"Write my class variable names separated by spaces on the argument."

	self classVarNames
		do: [ :each | aStream nextPutAll: each ]
		separatedBy: [ aStream space ]
]

{ #category : #printing }
ClassDescription >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^ String streamContents: [ :stream | self classVariablesOn: stream ]
]

{ #category : #compiling }
ClassDescription >> compile: sourceCode classified: protocol [
	"Compile the source code in the context of the receiver and install the result in the receiver's method dictionary under the given protocol (this can be a protocol or a protocol name).
	
	The argument sourceCode is either a string or an object that converts to a string or a PositionableStream on an object that converts to a string."

	^ self compile: sourceCode classified: protocol notifying: nil
]

{ #category : #compiling }
ClassDescription >> compile: sourceCode classified: protcol notifying: requestor [

	^ self compile: sourceCode classified: protcol withStamp: Author changeStamp notifying: requestor
]

{ #category : #compiling }
ClassDescription >> compile: sourceCode classified: protocol withStamp: changeStamp notifying: requestor [

	^ self
		  compile: sourceCode
		  classified: protocol
		  withStamp: changeStamp
		  notifying: requestor
		  logSource: self acceptsLoggingOfCompilation
]

{ #category : #compiling }
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

{ #category : #compiling }
ClassDescription >> compile: sourceCode notifying: requestor [
	"Refer to the comment in Behavior|compile:notifying:."

	^ self compile: sourceCode classified: Protocol unclassified notifying: requestor
]

{ #category : #compiling }
ClassDescription >> compileSilently: sourceCode [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: sourceCode classified: 'not defined protocol' notifying: nil
]

{ #category : #compiling }
ClassDescription >> compileSilently: sourceCode classified: protocolName [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: sourceCode classified: protocolName notifying: nil
]

{ #category : #compiling }
ClassDescription >> compileSilently: sourceCode classified: protocolName notifying: requestor [
	"Compile the code and classify the resulting method in the given protocol, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list."

	^ SystemAnnouncer uniqueInstance suspendAllWhile: [ self compile: sourceCode classified: protocolName notifying: requestor ]
]

{ #category : #copying }
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

{ #category : #slots }
ClassDescription >> definesSlot: aSlot [
	"Return true whether the receiver defines an instance variable named aString"

	^ self slots identityIncludes: aSlot
]

{ #category : #slots }
ClassDescription >> definesSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."

	^ self slotNames includes: aString
]

{ #category : #'file in/out' }
ClassDescription >> definition [
	"Answer a String that defines the receiver."
	self
		deprecated: #definition
		transformWith: '`@receiver definition' -> '`@receiver definitionString'.
	^ self definitionString
]

{ #category : #fileout }
ClassDescription >> definitionPrinter [
	"Return a configurated printer associated with the current class definition format."

	^ ClassDefinitionPrinter for: self
]

{ #category : #fileout }
ClassDescription >> definitionString [
	"Return the string of the class definition, each of my subclass may tell the printer how to printer it. A kind of double dispatch since we have multiple printers and multiple entities to be printed."

	^ self definitionPrinter definitionString
]

{ #category : #fileout }
ClassDescription >> definitionStringFor: aConfiguredPrinter [

	^ self subclassResponsibility
]

{ #category : #'queries - protocols' }
ClassDescription >> ensureProtocol: aProtocol [
	"I can take a Protocol or a protocol name as paramater.
	
	If my parameter is a name, I'll return a protocol associated with it. A new one if needed.
	If my parameter is a Protocol, I'll ensure that it comes from me, else I'll throw an error."

	^ self organization ensureProtocol: aProtocol
]

{ #category : #private }
ClassDescription >> errorCategoryName [
	self error: 'Category name must be a String'
]

{ #category : #fileout }
ClassDescription >> expandedDefinitionString [
	"Return the string of the class definition, each of my subclass may tell the printer how to printer it. A kind of double dispatch since we have multiple printers and multiple entities to be printed."

	^ self expandedDefinitionStringFor: (self definitionPrinter for: self)
]

{ #category : #fileout }
ClassDescription >> expandedDefinitionStringFor: aPrinter [

	^ self subclassResponsibility
]

{ #category : #'instance variables' }
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

{ #category : #'accessing - parallel hierarchy' }
ClassDescription >> hasClassSide [
	^self subclassResponsibility
]

{ #category : #'instance variables' }
ClassDescription >> hasInstVarNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."

	^ self instVarNames includes: aString
]

{ #category : #'pool variable' }
ClassDescription >> hasSharedPools [
	"Only a class may have shared pools"
	^ false
]

{ #category : #slots }
ClassDescription >> hasSlot: aSlot [
	"Return true whether the receivers hierarchy defines an instance variable named aString."

	^ self allSlots identityIncludes: aSlot
]

{ #category : #slots }
ClassDescription >> hasSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString.
	this includes non-visible slots"

	^ self classLayout hasSlotNamed: aString
]

{ #category : #'pool variable' }
ClassDescription >> includesSharedPoolNamed:  aSharedPoolString [
	"Answer whether the receiver uses the shared pool named aSharedPoolString"

	^ self sharedPools anySatisfy: [:each | each name = aSharedPoolString]
]

{ #category : #'instance variables' }
ClassDescription >> instVarIndexFor: instVarName [
	"Answer the index of the named instance variable."

	^self instVarIndexFor: instVarName ifAbsent: 0
]

{ #category : #'instance variables' }
ClassDescription >> instVarIndexFor: instVarName ifAbsent: aBlock [
	"Answer the index of the named instance variable."

	^self
		slotNamed: instVarName
		ifFound: [ :slot | slot isVirtual ifTrue: [aBlock value] ifFalse: [slot index]]
		ifNone: aBlock
]

{ #category : #private }
ClassDescription >> instVarMappingFrom: oldClass [
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames := oldClass allInstVarNames.
	^self allInstVarNames
			collect: [:instVarName | oldInstVarNames indexOf: instVarName]
]

{ #category : #'instance variables' }
ClassDescription >> instVarNames [
	"Answer an Array of the receiver's instance variable names."

	^self slots collect: [ :each | each name ]
]

{ #category : #'accessing - parallel hierarchy' }
ClassDescription >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	"Point instanceSide >>> Point"
	"Point class instanceSide >>> Point"

	^ self subclassResponsibility
]

{ #category : #printing }
ClassDescription >> instanceVariablesOn: aStream [
	"Write my instance variable names separated by spaces on the argument."

	self localSlots
		do: [ :each | aStream nextPutAll: each name asString]
		separatedBy: [ aStream space ]
]

{ #category : #printing }
ClassDescription >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^ String streamContents: [ :stream | self instanceVariablesOn: stream ]
]

{ #category : #'accessing - parallel hierarchy' }
ClassDescription >> isClassSide [
	"Return true whether the receiver is a metaclass (in a couple class/metaclass sense)."
	"Point isClassSide >>> false"
	"Point class isClassSide >>> true"

	^self == self classSide
]

{ #category : #testing }
ClassDescription >> isDeprecated [
	^ self package isDeprecated
]

{ #category : #'accessing - parallel hierarchy' }
ClassDescription >> isInstanceSide [
	"Return true whether the receiver is a class (in a couple class/metaclass sense)."
	"Point isInstanceSide >>> true"
	"Point class isInstanceSide >>> false"

	^ self isClassSide not
]

{ #category : #testing }
ClassDescription >> isLocalMethodsProtocol: aProtocol [
	aProtocol methodSelectors ifEmpty: [ ^ true ].

	^ aProtocol methodSelectors anySatisfy: [ :each | self isLocalSelector: each ]
]

{ #category : #testing }
ClassDescription >> isLocalSelector: aSelector [

	^ self methodDict includesKey: aSelector
]

{ #category : #'accessing - deprecated parallel hierarchy' }
ClassDescription >> isMeta [
	^self isClassSide
]

{ #category : #testing }
ClassDescription >> isTaggedWith: aSymbol [

	^ self packageTag
		  ifNil: [ false ]
		  ifNotNil: [ :tag | tag = aSymbol ]
]

{ #category : #private }
ClassDescription >> linesOfCode [
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."

	| lines |
	lines := self localMethods inject: 0 into: [ :sum :each | sum + each linesOfCode ].
	^ self isMeta
		ifTrue: [ lines ]
		ifFalse: [ lines + self classSide linesOfCode ]
]

{ #category : #slots }
ClassDescription >> localSlots [

	^ self slots select: [ :aSlot | aSlot isDefinedByOwningClass ]
]

{ #category : #organization }
ClassDescription >> methodsInProtocol: aString [

	^ (self organization methodSelectorsInProtocol: aString) collect: [ :each | self compiledMethodAt: each ]
]

{ #category : #'accessing - tags' }
ClassDescription >> methodsTaggedWith: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose.
	And class contains all method tags which used or not used yet by methods.
	This method select all methods marked by given tag.
	Tags are not inherited from traits. So we only select local methods"

	^self localMethods select: [ :each | each isTaggedWith: aSymbol ]
]

{ #category : #slots }
ClassDescription >> needsSlotClassDefinition [
    "return true if we define something else than InstanceVariableSlots or normal class variables"

    ^ self slotsNeedFullDefinition or: [ self class slotsNeedFullDefinition ]
]

{ #category : #private }
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

{ #category : #private }
ClassDescription >> newInstanceFrom: oldInstance variable: variable size: instSize map: map [
	"Create a new instance of the receiver based on the given old instance.
	The supplied map contains a mapping of the old instVar names into
	the receiver's instVars"
	| new |
	variable
		ifTrue: [new := self basicNew: oldInstance basicSize]
		ifFalse: [new := self basicNew].
	1 to: instSize do:
		[:offset |  (map at: offset) > 0 ifTrue:
			[new instVarAt: offset
					put: (oldInstance instVarAt: (map at: offset))]].
	variable
		ifTrue: [1 to: oldInstance basicSize do:
					[:offset |
					new basicAt: offset put: (oldInstance basicAt: offset)]].
	^new
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> noteAddedSelector: aSelector meta: isMeta [
	"A hook allowing some classes to react to adding of certain selectors"
]

{ #category : #compiling }
ClassDescription >> noteCompilationOf: aSelector meta: isMeta [
	"A hook allowing some classes to react to recompilation of certain selectors"
]

{ #category : #'organization updating' }
ClassDescription >> notifyOfRecategorizedSelector: selector from: oldProtocol to: newProtocol [
	"If compiled method is not there, it meens it has been removed, not recategorized... so I skip
	 the method recategorized announce"

	self
		compiledMethodAt: selector
		ifPresent: [ :method | SystemAnnouncer uniqueInstance methodRecategorized: method oldProtocol: (oldProtocol ifNotNil: [ oldProtocol name ]) ]
]

{ #category : #private }
ClassDescription >> notifyRepackage: selector method: compiledMethod oldProtocol: oldProtocol newProtocol: newProtocol [

	| oldPackage newPackage |
	newProtocol = oldProtocol ifTrue: [ ^ self ].

	"This indirection is because we need to abstract RPackage from the kernel"
	self class environment at: #RPackage ifPresent: [ :rPackageClass |
		newPackage := rPackageClass organizer packageForProtocol: newProtocol name inClass: self.
		oldPackage := rPackageClass organizer packageForProtocol: oldProtocol name inClass: self.

		"Announce recategorization"
		newPackage = oldPackage ifFalse: [ SystemAnnouncer uniqueInstance methodRepackaged: compiledMethod from: oldPackage to: newPackage ] ].

	SystemAnnouncer uniqueInstance methodRecategorized: compiledMethod oldProtocol: oldProtocol name
]

{ #category : #private }
ClassDescription >> numberOfMethods [
	"count all methods that are local (not comming from a trait)"
	| num |
	num := self localMethods size.
	^ self isMeta
		ifTrue: [ num ]
		ifFalse: [ num + self classSide numberOfMethods ]
]

{ #category : #initialization }
ClassDescription >> obsolete [
	"Make the receiver obsolete."
	self superclass removeSubclass: self.
	super obsolete
]

{ #category : #'file in/out' }
ClassDescription >> oldDefinition [

	^ ClassDefinitionPrinter legacy
		for: self;
		definitionString
]

{ #category : #'file in/out' }
ClassDescription >> oldPharoDefinition [

	^ ClassDefinitionPrinter oldPharo
		for: self;
		definitionString
]

{ #category : #organization }
ClassDescription >> organization [
	"Answer the instance of ClassOrganizer that represents the organization
	of the messages of the receiver."

	^ organization ifNil: [
		  self organization: ClassOrganization new.
		  self isTrait ifFalse: [ organization initializeClass: self ].
		  organization ]
]

{ #category : #organization }
ClassDescription >> organization: aClassOrg [
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [ aClassOrg setSubject: self ].
	organization := aClassOrg
]

{ #category : #printing }
ClassDescription >> printOn: aStream [
	aStream nextPutAll: self name
]

{ #category : #'accessing - protocols' }
ClassDescription >> protocolNames [
	"Return the list of all the protocol names included in this class."

	^ self organization protocolNames copy
]

{ #category : #compiling }
ClassDescription >> reformatAll [
	"Reformat all methods in this class"

	self methods do: [ :method | method reformat ]
]

{ #category : #'instance variables' }
ClassDescription >> removeInstVarNamed: aString [
	"Remove the argument, aString, as one of the receiver's instance variables."

	^self removeSlot: (self slotNamed: aString)
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> removeProtocol: protocolName [
	"Remove each of the messages categorized under aString in the method
	dictionary of the receiver. Then remove the category aString."

	(self organization methodSelectorsInProtocol: protocolName) do: [ :sel | self removeSelector: sel ].
	self organization removeProtocolIfEmpty: protocolName
]

{ #category : #'accessing - protocols' }
ClassDescription >> removeProtocolIfEmpty: aProtocol [
	"Remove a given protocol (the argument can also be a protocol name) if no method is present in it."

	self organization removeProtocolIfEmpty: aProtocol
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> removeSelector: selector [
	"Remove the message whose selector is given from the method
	dictionary of the receiver, if it is there. Answer nil otherwise."

	| priorMethod priorProtocol origin |
	priorMethod := self compiledMethodAt: selector ifAbsent: [^ nil].
	origin := priorMethod origin.
	priorProtocol := self whichCategoryIncludesSelector: selector.
	self organization removeElement: selector.

	super removeSelector: selector.

	SystemAnnouncer uniqueInstance methodRemoved: priorMethod protocol: priorProtocol origin: origin
]

{ #category : #'instance variables' }
ClassDescription >> removeSlot: aSlot [

	^self subclassResponsibility
]

{ #category : #organization }
ClassDescription >> reorganize [
	"During fileIn, !Rectangle reorganize! allows Rectangle to seize control and treat the next chunk as its organization.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"

	^self organization
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> selectorsInCategory: aName [
	"Answer a list of the selectors of the receiver that are in category named aName"

	^ (self organization methodSelectorsInProtocol: aName) asSet asArray sort
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> selectorsInProtocol: aName [
	"Answer a list of the selectors of the receiver that are in category named aName"

	^ (self organization methodSelectorsInProtocol: aName) asSet asArray sort
]

{ #category : #'pool variable' }
ClassDescription >> sharedPoolOfVarNamed: aString [
	"Only classes may have shared pools"

	^ nil
]

{ #category : #printing }
ClassDescription >> sharedPoolString [
	"Answer a string of my shared pools separated by spaces."

	^String streamContents: [ :stream |
		self sharedPoolStringOn: stream ]
]

{ #category : #printing }
ClassDescription >> sharedPoolStringOn: aStream [
	"Write my shared pools separated by dots on argument, aStream."

	self sharedPools do:  [ :p | aStream nextPutAll: p name ] separatedBy: [ aStream nextPutAll: ' . ' ]
]

{ #category : #'pool variable' }
ClassDescription >> sharedPools [
	^ OrderedCollection new
]

{ #category : #printing }
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

{ #category : #'pool variable' }
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

{ #category : #slots }
ClassDescription >> slotNamed: aName [
	^self classLayout resolveSlot: aName asSymbol
]

{ #category : #slots }
ClassDescription >> slotNamed: aName ifFound: foundBlock [
	^self slotNamed: aName ifFound: foundBlock ifNone: [ "do nothing" ]
]

{ #category : #slots }
ClassDescription >> slotNamed: aName ifFound: foundBlock ifNone: exceptionBlock [
	^self classLayout resolveSlot: aName ifFound: foundBlock ifNone: exceptionBlock
]

{ #category : #slots }
ClassDescription >> slotNames [
	^self slots collect: [ :each | each name ]
]

{ #category : #accessing }
ClassDescription >> slots [
	^self classLayout visibleSlots
]

{ #category : #slots }
ClassDescription >> slotsNeedFullDefinition [
	"return true if we define something else than InstanceVariableSlots"
	^self slots anySatisfy: [ :each | each needsFullDefinition ]
]

{ #category : #private }
ClassDescription >> spaceUsed [
	^super spaceUsed + (self hasClassSide
		ifTrue: [self classSide spaceUsed]
		ifFalse: [0])
]

{ #category : #printing }
ClassDescription >> storeOn: aStream [
	"Classes and Metaclasses have global names."

	aStream nextPutAll: self name
]

{ #category : #initialization }
ClassDescription >> superclass: aSuperclass layout: aLayout [
	layout := aLayout.

	self
		superclass: aSuperclass
		methodDictionary: self emptyMethodDictionary
		format: aLayout format
]

{ #category : #initialization }
ClassDescription >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	self organization: nil
]

{ #category : #initialization }
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

{ #category : #'accessing - tags' }
ClassDescription >> tagWith: aSymbol [
	| package packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we could only model single tag with RPackageTag"

	package := self package.
	packageTag := package addClassTag: aSymbol.
	packageTag addClass: self
]

{ #category : #'accessing - tags' }
ClassDescription >> tags [

	| packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we only define API to manage them on top of RPackageTag"
	packageTag := self package classTagForClass: self.
	packageTag ifNil: [ ^ #(  ) ].
	packageTag isRoot ifTrue: [ ^ #(  ) ].

	^ { packageTag name }
]

{ #category : #'accessing - tags' }
ClassDescription >> tagsForMethods [
	"Any method could be tagged with multiple symbols for user purpose.
	And class contains all method tags which used or not used yet by methods.
	For now we could only implemented it on top of Protocol.
	It supposed to not include any method tags inherited from Traits
	which is opposite to current Protocol implementation.
	And extension protocol is not treated as tag"
	| allProtocols |
	allProtocols := self organization protocols reject: [ :each | each isUnclassifiedProtocol | each isExtensionProtocol ].

	^ allProtocols select: [ :each | self isLocalMethodsProtocol: each ] thenCollect: #name
]

{ #category : #'accessing - deprecated parallel hierarchy' }
ClassDescription >> theMetaClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #classSide instead' transformWith:  '`@receiver theMetaClass'
						-> '`@receiver classSide'.
	^ self classSide
]

{ #category : #'accessing - deprecated parallel hierarchy' }
ClassDescription >> theNonMetaClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #instanceSide instead' transformWith:  '`@receiver theNonMetaClass'
						-> '`@receiver instanceSide'.
	^ self instanceSide
]

{ #category : #'accessing - method dictionary' }
ClassDescription >> uncategorizedSelectors [
	^ self selectorsInProtocol: Protocol unclassified
]

{ #category : #'accessing - tags' }
ClassDescription >> untagFrom: aSymbol [
	| package packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we could only model single tag with RPackageTag"
	package := self package.
	packageTag := package classTagForClass: self.
	packageTag ifNil: [ ^#() ].
	packageTag isRoot ifTrue: [ ^#() ].
	packageTag name = aSymbol ifFalse: [ ^self ].
	packageTag removeClass: self.
	package addClass: self
]

{ #category : #'pool variable' }
ClassDescription >> usesLocalPoolVarNamed: aString [
	^false
]

{ #category : #'pool variable' }
ClassDescription >> usesPoolVarNamed: aString [
	"Only classes may use a pool variable named: aString"

	^ false
]

{ #category : #compiling }
ClassDescription >> wantsChangeSetLogging [
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism."

	^ true
]

{ #category : #organization }
ClassDescription >> whichCategoryIncludesSelector: aSelector [
	"Answer the category of the argument, aSelector, in the organization of
	the receiver, or answer nil if the receiver does not inlcude this selector."

	^ self organization protocolNameOfElement: aSelector
]

{ #category : #queries }
ClassDescription >> whichSelectorsAccess: instVarName [
	"Answer the selectors whose methods access the argument, instVarName, as a named instance variable."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self selectors select: [ :sel | slot isAccessedIn: self>>sel ] ]
		ifNone: [ #() ]
]

{ #category : #queries }
ClassDescription >> whichSelectorsRead: instVarName [
	"Answer the selectors whose methods read the argument, instVarName, as a named instance variable."

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self selectors select: [ :sel | slot isReadIn: self >> sel ] ]
		ifNone: [ #() ]
]

{ #category : #queries }
ClassDescription >> whichSelectorsWrite: instVarName [
	"Answer the selectors whose methods write the argument, instVarName, as a named instance variable"

	^ self
		slotNamed: instVarName
		ifFound: [ :slot | self selectors select: [ :sel | slot isWrittenIn: self >> sel ] ]
		ifNone: [ #() ]
]

{ #category : #organization }
ClassDescription >> zapOrganization [
	"Remove the organization of this class by message categories.
	This is typically done to save space in small systems.  Classes and methods
	created or filed in subsequently will, nonetheless, be organized"

	self organization: nil.
	self isClassSide ifFalse: [self classSide zapOrganization]
]
