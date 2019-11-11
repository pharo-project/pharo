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

{ #category : #'accessing method dictionary' }
ClassDescription >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category [
	| priorMethodOrNil priorOriginOrNil oldProtocol newProtocol |

	priorMethodOrNil := self 
		compiledMethodAt: selector 
		ifAbsent: [ nil ].
	priorMethodOrNil ifNotNil: [ priorOriginOrNil := priorMethodOrNil origin ].
	
	self addSelectorSilently: selector withMethod: compiledMethod.
	
	oldProtocol := self organization categoryOfElement: selector.
	SystemAnnouncer uniqueInstance suspendAllWhile: [ 
		self organization 
			classify: selector 
			under: (category = Protocol unclassified 
				ifTrue: [ oldProtocol ]
				ifFalse: [ category ]) ].
	newProtocol := self organization categoryOfElement: selector.
	
	self isAnonymous ifTrue: [ ^ self ].
	
	(priorMethodOrNil isNil or: [ priorOriginOrNil ~= compiledMethod origin ])
		ifTrue: [ SystemAnnouncer uniqueInstance methodAdded: compiledMethod ]
		ifFalse: [ 
			"If protocol changed and someone is from different package, I need to throw a method recategorized"
			self
				notifyRepackage: selector
				method: compiledMethod
				oldProtocol: oldProtocol
				newProtocol: newProtocol.	
					
			SystemAnnouncer uniqueInstance
				methodChangedFrom: priorMethodOrNil
				to: compiledMethod
				oldProtocol: oldProtocol ]
]

{ #category : #organization }
ClassDescription >> addCategory: newName before: aCategory [

	^ self organization addCategory: newName before: aCategory 
]

{ #category : #'instance variables' }
ClassDescription >> addInstVarNamed: aString [ 
	"Add the argument, aString, as one of the receiver's instance variables."

	self subclassResponsibility
]

{ #category : #'accessing tags' }
ClassDescription >> addMethodTag: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose.
	In class we could define what tags could be used for methods"
	self organization addCategory: aSymbol
]

{ #category : #'accessing method dictionary' }
ClassDescription >> addSelector: selector withMethod: compiledMethod [
	| priorMethodOrNil oldProtocol |
	priorMethodOrNil := self compiledMethodAt: selector ifAbsent: [nil].
	oldProtocol := priorMethodOrNil ifNotNil: [ priorMethodOrNil protocol ].
	self addSelectorSilently: selector withMethod: compiledMethod.
	priorMethodOrNil 
		ifNil: [SystemAnnouncer uniqueInstance methodAdded: compiledMethod ]
		ifNotNil: [SystemAnnouncer uniqueInstance methodChangedFrom: priorMethodOrNil to: compiledMethod oldProtocol: oldProtocol ].
]

{ #category : #'accessing method dictionary' }
ClassDescription >> addSelectorSilently: selector withMethod: compiledMethod [
	super addSelectorSilently: selector withMethod: compiledMethod.
	self instanceSide noteAddedSelector: selector meta: self isMeta.
]

{ #category : #'instance variables' }
ClassDescription >> addSlot: aSlot [

	^self subclassResponsibility 
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

{ #category : #'accessing method dictionary' }
ClassDescription >> allProtocolsUpTo: mostGenericClass [
	"Answer a list of all the method protocols of the receiver and all its superclasses, up through mostGenericClass"

	| otherClassCategories thisClassCategories lowercaseSortBlock  |
		
	otherClassCategories := OrderedCollection new.
	lowercaseSortBlock  := [ :a :b | a asLowercase <= b asLowercase ].
	
	(self allSuperclassesIncluding: mostGenericClass) do: [ :aClass | 
		 otherClassCategories addAll: aClass organization categories ].
	otherClassCategories remove: 'no messages' ifAbsent: [  ].
	thisClassCategories := self organization categories sorted: lowercaseSortBlock.
													
	^ thisClassCategories , ((otherClassCategories asSet removeAllSuchThat: [ :each |
								thisClassCategories includes: each ]) sorted: lowercaseSortBlock)
]

{ #category : #'accessing method dictionary' }
ClassDescription >> allSelectorsInProtocol: aName [ 
	"Answer a list of all the methods of the receiver and all its 
	superclasses that are in the protocol named aName"
	
	^self withAllSuperclasses
		flatCollectAsSet: [ :class | class organization listAtCategoryNamed: aName ]
		
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

{ #category : #'accessing - deprecated parallel hierarchy ' }
ClassDescription >> baseClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #instanceSide instead' transformWith:  '`@receiver baseClass' 
						-> '`@receiver instanceSide'.
	^ self instanceSide
]

{ #category : #organization }
ClassDescription >> basicOrganization [
	^ organization 
]

{ #category : #organization }
ClassDescription >> basicOrganization: aClassOrg [
	organization := aClassOrg
]

{ #category : #'accessing - deprecated parallel hierarchy ' }
ClassDescription >> classClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #classSide instead' transformWith:  '`@receiver classClasss' 
						-> '`@receiver classSide'.
	^ self classSide
]

{ #category : #'file in/out' }
ClassDescription >> classComment: aString [
	"Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."
	^ self classComment: aString stamp: '<historical>'
]

{ #category : #'file in/out' }
ClassDescription >> classComment: aString stamp: aStamp [
	"Store the comment, aString or Text or RemoteString, associated with the class we are organizing.  Empty string gets stored only if had a non-empty one before."

	| pointer header oldCommentRemoteString oldComment oldStamp preamble |
	oldComment := self organization classComment.
	oldStamp := self organization commentStamp.

	aString string = oldComment string ifTrue: [ ^ self ].

	aString isRemoteString ifTrue: [
		SystemAnnouncer uniqueInstance 
			class: self 
			oldComment: oldComment 
			newComment: aString string 
			oldStamp: oldStamp 
			newStamp: aStamp.
		^ self organization classComment: aString stamp: aStamp].

	oldCommentRemoteString := self organization commentRemoteString.
	pointer := oldCommentRemoteString 
			ifNil: [0] 
			ifNotNil: [oldCommentRemoteString sourcePointer].
	
	preamble := String streamContents: [ :file |
		file cr; nextPut: $!.
		header := String streamContents: [:strm | 
			strm 
				nextPutAll: self name;
				nextPutAll: ' commentStamp: '.
			aStamp storeOn: strm.
			strm nextPutAll: ' prior: '; nextPutAll: pointer printString ].
		file nextChunkPut: header; cr ].

	SourceFiles
		writeSource: aString
		preamble: preamble
		onSuccess: [ :newSourcePointer | 
			self organization 
				classComment: (SourceFiles remoteStringAt: newSourcePointer) 
				stamp: aStamp ]
		onFail: [ "ignore" ].
	
	SystemAnnouncer uniqueInstance 
		class: self 
		oldComment: oldComment 
		newComment: aString 
		oldStamp: oldStamp 
		newStamp: aStamp
]

{ #category : #'accessing comment' }
ClassDescription >> classCommentBlank [

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
			
	(self instVarNames size > 0)
		 ifTrue: [stream cr; cr; nextPutAll: '    Instance Variables'.  ].
		
	self instVarNames asSortedCollection do: [:each |
		stream
			cr; tab; nextPutAll: each;
			nextPut: $:;
			tab; tab;
			nextPutAll: '<Object>'].
	stream cr.
stream cr; cr; nextPutAll: '    Implementation Points'.
	^stream contents
]

{ #category : #'accessing parallel hierarchy' }
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

{ #category : #slots }
ClassDescription >> classVariablesNeedFullDefinition [

	^ self classVariables anySatisfy: [ :each | each needsFullDefinition ]
]

{ #category : #printing }
ClassDescription >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^String streamContents: [ :stream | 
		self classVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #'accessing class hierarchy' }
ClassDescription >> classesThatImplementAllOf: selectorSet [
	"Return an array of any classes that implement all the messages in selectorSet."

	| found remaining |
	found := OrderedCollection new.
	selectorSet do:
		[:sel | (self includesSelector: sel) ifTrue: [found add: sel]].
	found isEmpty
		ifTrue: [^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: selectorSet)]]
		ifFalse: [remaining := selectorSet copyWithoutAll: found.
				remaining isEmpty ifTrue: [^ Array with: self].
				^ self subclasses inject: Array new
						into: [:subsThatDo :sub |
							subsThatDo , (sub classesThatImplementAllOf: remaining)]]
]

{ #category : #'accessing comment' }
ClassDescription >> comment [
    "Answer the receiver's comment. (If missing, supply a template) "
    ^self instanceSide organization classComment ifEmpty: [ self classCommentBlank ]
]

{ #category : #'accessing comment' }
ClassDescription >> comment: aStringOrText [
	"Set the receiver's comment to be the argument, aStringOrText."

	self instanceSide classComment: aStringOrText.
]

{ #category : #'accessing comment' }
ClassDescription >> comment: aStringOrText stamp: aStamp [
	"Set the receiver's comment to be the argument, aStringOrText."

	self instanceSide classComment: aStringOrText stamp: aStamp.
]

{ #category : #'file in/out' }
ClassDescription >> commentStamp: changeStamp [
	self organization commentStamp: changeStamp
]

{ #category : #compiling }
ClassDescription >> compile: code classified: heading [ 
	"Compile the argument, code, as source code in the context of the 
	receiver and install the result in the receiver's method dictionary under 
	the classification indicated by the second argument, heading. nil is to be 
	notified if an error occurs. The argument code is either a string or an 
	object that converts to a string or a PositionableStream on an object that 
	converts to a string."

	^self
		compile: code
		classified: heading
		notifying: nil
]

{ #category : #compiling }
ClassDescription >> compile: text classified: category notifying: requestor [
	| stamp |
	stamp := self acceptsLoggingOfCompilation
		ifTrue: [ Author changeStamp ]
		ifFalse: [ nil ].
	^ self
		compile: text
		classified: category
		withStamp: stamp
		notifying: requestor
]

{ #category : #compiling }
ClassDescription >> compile: text classified: category withStamp: changeStamp notifying: requestor [
	^ self
		compile: text
		classified: category
		withStamp: changeStamp
		notifying: requestor
		logSource: self acceptsLoggingOfCompilation
]

{ #category : #compiling }
ClassDescription >> compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource [
 
	| method selector |

	method := self compiler
		source: text;
		requestor: requestor;
		failBlock:  [ ^nil ];
		compile.
	
	selector := method selector.
	logSource ifTrue: [
		self 
			logMethodSource: (requestor ifNotNil: [ :r | r text ] ifNil: [ text ]) "the requestor text might have been changed by the compiler and may be different thant text argument"
			forMethod: method
			inCategory: category 
			withStamp: changeStamp].
		
	self 
		addAndClassifySelector: selector 
		withMethod: method 
		inProtocol: category.
			
	self instanceSide 
		noteCompilationOfMethod: method 
		meta: self isClassSide.

	^ selector
]

{ #category : #compiling }
ClassDescription >> compile: code notifying: requestor [ 
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: Protocol unclassified
		 notifying: requestor
]

{ #category : #compiling }
ClassDescription >> compileSilently: code classified: category [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: category notifying: nil.
]

{ #category : #compiling }
ClassDescription >> compileSilently: code classified: category notifying: requestor [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ SystemAnnouncer uniqueInstance 
		suspendAllWhile: [self compile: code classified: category notifying: requestor].
]

{ #category : #copying }
ClassDescription >> copy: sel from: class [ 
	"Install the method associated with the first argument, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under -As yet not 
	classified-."

	self copy: sel
		from: class
		classified: nil
]

{ #category : #copying }
ClassDescription >> copy: sel from: class classified: cat [ 
	"Install the method associated with the first arugment, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under the third 
	argument, cat."

	| code category |
	"Useful when modifying an existing class"
	code := class sourceCodeAt: sel.
	code ifNotNil: 
			[cat
				ifNil: [category := class organization categoryOfElement: sel]
				ifNotNil: [category := cat].
			(self includesLocalSelector: sel)
				ifTrue: [code asString = (self sourceCodeAt: sel) asString 
							ifFalse: [self error: self name 
										, ' ' 
										, sel 
										, ' will be redefined if you proceed.']].
			self compile: code classified: category]
]

{ #category : #copying }
ClassDescription >> copyAll: selArray from: class [ 
	"Install all the methods found in the method dictionary of the second 
	argument, class, as the receiver's methods. Classify the messages under 
	-As yet not classified-."

	self copyAll: selArray
		from: class
		classified: nil
]

{ #category : #copying }
ClassDescription >> copyAll: selArray from: class classified: cat [ 
	"Install all the methods found in the method dictionary of the second 
	argument, class, as the receiver's methods. Classify the messages under 
	the third argument, cat."

	selArray do: [:s |
		(class includesLocalSelector: s) ifTrue: [
			self copy: s
				from: class
				classified: cat ] ]
]

{ #category : #copying }
ClassDescription >> copyAllCategoriesFrom: aClass [ 
	"Specify that the categories of messages for the receiver include all of 
	those found in the class, aClass. Install each of the messages found in 
	these categories into the method dictionary of the receiver, classified 
	under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]
]

{ #category : #copying }
ClassDescription >> copyCategory: cat from: class [ 
	"Specify that one of the categories of messages for the receiver is cat, as 
	found in the class, class. Copy each message found in this category."

	self copyCategory: cat
		from: class
		classified: cat
]

{ #category : #copying }
ClassDescription >> copyCategory: cat from: aClass classified: newCat [ 
	"Specify that one of the categories of messages for the receiver is the 
	third argument, newCat. Copy each message found in the category cat in 
	class aClass into this new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat
]

{ #category : #'file in/out' }
ClassDescription >> definition [
	"Answer a String that defines the receiver."

	(self needsSlotClassDefinition or: [ Slot showSlotClassDefinition ]) ifTrue: [ 
		^ self definitionWithSlots ].
	
	^ self definitionWithoutSlots
	
]

{ #category : #accessing }
ClassDescription >> definitionForNautilus [

	^ self definition
]

{ #category : #'file in/out' }
ClassDescription >> definitionWithSlots [
	"The class definition with a way to specify slots. Shown when the class defines special Slot"
	
	| stream poolString|
	poolString := self sharedPoolsString.

	stream := (String new: 800) writeStream.
	superclass 
		ifNotNil: [stream nextPutAll: superclass name]
		ifNil: [stream nextPutAll: 'ProtoObject'].
	stream 
		nextPutAll: ' subclass: '; 
		store: self name.
	self hasTraitComposition ifTrue: [
		stream 
			crtab; 
			nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString ].
			
	(self classLayout isKindOf: FixedLayout) ifFalse: [
		stream 
			crtab; 
			nextPutAll: 'layout: ';
			nextPutAll: self classLayout class name ].
	
	stream 
		crtab; 
		nextPutAll: 'slots: ';
		nextPutAll: self slotDefinitionString.
		
	stream 
		crtab; 
		nextPutAll: 'classVariables: ';
		nextPutAll: self classVariableDefinitionString.
	
	poolString = '' ifFalse: [
		stream 
			crtab; 
			nextPutAll: 'poolDictionaries: ';
			store: poolString ].
		
	stream 
		crtab; 
		nextPutAll: 'package: ';
		store: self category asString.

	superclass ifNil: [ 
		stream nextPutAll: '.'; cr.
		stream nextPutAll: self name.
		stream space; nextPutAll: 'superclass: nil'. ].

	^ stream contents
]

{ #category : #'file in/out' }
ClassDescription >> definitionWithoutSlots [

	| poolString stream |
	poolString := self sharedPoolsString.
	stream := (String new: 800) writeStream.
	superclass
		ifNotNil: [ stream nextPutAll: superclass name ]
		ifNil: [ stream nextPutAll: 'ProtoObject' ].
	
	stream
		nextPutAll: self kindOfSubclass;
		store: self name.
	self hasTraitComposition ifTrue: [ 
		stream
			crtab;
			nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString ].
	stream
		crtab;
		nextPutAll: 'instanceVariableNames: ';
		store: self instanceVariablesString.
	stream
		crtab;
		nextPutAll: 'classVariableNames: ';
		store: self classVariablesString.
	
	poolString = '' ifFalse: [ 
		stream 
			crtab;
			nextPutAll: 'poolDictionaries: ';
			store: poolString ].
	
	stream
		crtab;
		nextPutAll: 'package: ';
		store: self category asString.
		
	superclass ifNil: [ 
		stream
			nextPutAll: '.';
			cr.
		stream 
			nextPutAll: self name.
		stream
			space;
			nextPutAll: 'superclass: nil' ].

	^ stream contents
	
]

{ #category : #private }
ClassDescription >> errorCategoryName [
	self error: 'Category name must be a String'
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

{ #category : #'accessing parallel hierarchy' }
ClassDescription >> hasClassSide [
	^self subclassResponsibility.
]

{ #category : #'accessing comment' }
ClassDescription >> hasComment [
	"Return whether this class truly has a comment other than the default"
	^self instanceSide organization hasComment
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
		ifFound: [ :slot | slot isVirtual ifTrue: aBlock ifFalse: [slot index]]
		ifNone: aBlock
]

{ #category : #private }
ClassDescription >> instVarMappingFrom: oldClass [
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames := oldClass allInstVarNames.
	^self allInstVarNames 
			collect: [:instVarName | oldInstVarNames indexOf: instVarName].
]

{ #category : #'instance variables' }
ClassDescription >> instVarNames [
	"Answer an Array of the receiver's instance variable names."

	^self slots collect: [ :each | each name ]
]

{ #category : #'accessing parallel hierarchy' }
ClassDescription >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	"Point instanceSide >>> Point"
	"Point class instanceSide >>> Point"
	
	^ self subclassResponsibility
]

{ #category : #accessing }
ClassDescription >> instanceVariables [
	"we deprecate this due to bad naming: it returns names, not variables!"
	self
		deprecated: 'Please use #instVarNames or #slotNames instead'
		transformWith: '`@receiver instanceVariables' -> '`@receiver instVarNames'.
	^self instVarNames
]

{ #category : #printing }
ClassDescription >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self localSlots
			do: [ :each | stream nextPutAll: each name ]
			separatedBy: [ stream space ] ]
]

{ #category : #'accessing parallel hierarchy' }
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

{ #category : #'accessing parallel hierarchy' }
ClassDescription >> isInstanceSide [
	"Return true whether the receiver is a class (in a couple class/metaclass sense)."
	"Point isClassSide >>> false"
	"Point class isClassSide >>> true"
	
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

{ #category : #'accessing - deprecated parallel hierarchy ' }
ClassDescription >> isMeta [
	^self isClassSide
]

{ #category : #testing }
ClassDescription >> isTaggedWith: aSymbol [
	^self tags includes: aSymbol 
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

	^ self slots select: [ :aSlot | aSlot isLocal ]
]

{ #category : #'accessing tags' }
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

    ^ self slotsNeedFullDefinition 
		or: [ self class slotsNeedFullDefinition 
		or: [ self classVariablesNeedFullDefinition ] ]
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

{ #category : #'accessing method dictionary' }
ClassDescription >> noteAddedSelector: aSelector meta: isMeta [
	"A hook allowing some classes to react to adding of certain selectors"
]

{ #category : #compiling }
ClassDescription >> noteCompilationOf: aSelector meta: isMeta [
	"A hook allowing some classes to react to recompilation of certain selectors"
]

{ #category : #compiling }
ClassDescription >> noteCompilationOfMethod: aCompiledMethod meta: isMeta [
	self noteCompilationOf: aCompiledMethod selector meta: isMeta
]

{ #category : #'organization updating' }
ClassDescription >> notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory [
	"If compiled method is not there, it meens it has been removed, not recategorized... so I skip 
	 the method recategorized announce"
	(self compiledMethodAt: selector ifAbsent: [ nil ]) 
		ifNotNil: [ :method | 
			SystemAnnouncer uniqueInstance 
				methodRecategorized: method
				oldProtocol: oldCategory ].

]

{ #category : #private }
ClassDescription >> notifyRepackage: selector method: compiledMethod oldProtocol: oldProtocol newProtocol: newProtocol [
	| oldPackage newPackage |
	
	(newProtocol = oldProtocol)
		ifTrue: [ ^ self ].
		
	"This indirection is because we need to abstract RPackage from the kernel"
	self class environment at: #RPackage ifPresent: [ :rPackageClass | 
			newPackage := rPackageClass organizer 
				packageForProtocol: newProtocol 
				inClass: self.
			oldPackage := rPackageClass organizer 
				packageForProtocol: oldProtocol 
				inClass: self.
			
			"Announce recategorization"
			newPackage = oldPackage
				ifFalse: [ 
					SystemAnnouncer uniqueInstance 
						methodRepackaged: compiledMethod 
						from: oldPackage 
						to: newPackage ] ].	
	
	SystemAnnouncer uniqueInstance methodRecategorized: compiledMethod oldProtocol: oldProtocol
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
	super obsolete.
]

{ #category : #'file in/out' }
ClassDescription >> oldDefinition [
	"Answer a String that defines the receiver."

	| aStream |
	aStream := (String new: 800) writeStream.
	superclass 
		ifNil: [aStream nextPutAll: 'ProtoObject']
		ifNotNil: [aStream nextPutAll: superclass name].
	aStream nextPutAll: self kindOfSubclass;
			store: self name.
	(self hasTraitComposition) ifTrue: [
		aStream cr; tab; nextPutAll: 'uses: ';
			nextPutAll: self traitCompositionString].
	aStream cr; tab; nextPutAll: 'instanceVariableNames: ';
			store: self instanceVariablesString.
	aStream cr; tab; nextPutAll: 'classVariableNames: ';
			store: self classVariablesString.
	aStream cr; tab; nextPutAll: 'poolDictionaries: ';
			store: self sharedPoolsString.
	aStream cr; tab; nextPutAll: 'category: ';
			store: self category asString.

	superclass ifNil: [ 
		aStream nextPutAll: '.'; cr.
		aStream nextPutAll: self name.
		aStream space; nextPutAll: 'superclass: nil'. ].

	^ aStream contents
]

{ #category : #organization }
ClassDescription >> organization [
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	self basicOrganization
		ifNil: [ self basicOrganization: (self isTrait
						ifTrue: [ ClassOrganization new ]
						ifFalse: [ ClassOrganization forClass: self ]) ].
	^self basicOrganization setSubject: self.	"Making sure that subject is set correctly. It should not be necessary."
	
	
]

{ #category : #organization }
ClassDescription >> organization: aClassOrg [
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	self basicOrganization: aClassOrg.
]

{ #category : #printing }
ClassDescription >> printOn: aStream [ 
	aStream nextPutAll: self name
]

{ #category : #'accessing class hierarchy' }
ClassDescription >> printSubclassesOn: aStream level: level [ 

		self printSubclassesOn: aStream level: level filter: nil
]

{ #category : #'accessing class hierarchy' }
ClassDescription >> printSubclassesOn: aStream level: level filter: filterCollection [
	"As part of the algorithm for printing a description of the receiver, print the
	subclass on the file stream, aStream, indenting level times. 
	An optional filterCollection restricts printing to contained classes only."
	
	| subclassNames |
	filterCollection ifNotNil: [ (filterCollection includes: self) ifFalse: [ ^self ]].
	aStream crtab: level.
	aStream nextPutAll: self name.
	aStream space; print: self instVarNames.
	self == Class
		ifTrue: 
			[aStream crtab: level + 1; nextPutAll: '[ ... all the Metaclasses ... ]'.
			^self].
	subclassNames := self subclasses asSortedCollection:[:c1 :c2| c1 name <= c2 name].
	"Print subclasses in alphabetical order"
	subclassNames do:
		[:subclass | subclass printSubclassesOn: aStream level: level + 1 filter: filterCollection]
]

{ #category : #compiling }
ClassDescription >> reformatAll [
	"Reformat all methods in this class"
	self methods do: [:method | method reformat]
]

{ #category : #'instance variables' }
ClassDescription >> removeInstVarNamed: aString [ 
	"Remove the argument, aString, as one of the receiver's instance variables."

	^self removeSlot: (self slotNamed: aString)
]

{ #category : #'accessing tags' }
ClassDescription >> removeMethodTag: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose.
	In class we could define what tags could be used for methods.
	Here we could remove what we not need"
	self organization removeCategory: aSymbol
]

{ #category : #'accessing method dictionary' }
ClassDescription >> removeProtocol: aString [ 
	"Remove each of the messages categorized under aString in the method 
	dictionary of the receiver. Then remove the category aString."
	| categoryName |
	categoryName := aString asSymbol.
	(self organization listAtCategoryNamed: categoryName) do:
		[:sel | self removeSelector: sel].
	self organization removeCategory: categoryName
]

{ #category : #'accessing method dictionary' }
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

{ #category : #'accessing method dictionary' }
ClassDescription >> selectorsInCategory: aName [ 
	"Answer a list of the selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: (self organization listAtCategoryNamed: aName).
	^ aColl asArray sort
]

{ #category : #'accessing method dictionary' }
ClassDescription >> selectorsInProtocol: aName [ 
	"Answer a list of the selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: (self organization listAtCategoryNamed: aName).
	^ aColl asArray sort
]

{ #category : #'pool variable' }
ClassDescription >> sharedPoolOfVarNamed: aString [
	"Only classes may have shared pools"
	
	^ nil
]

{ #category : #printing }
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

{ #category : #printing }
ClassDescription >> slotDefinitionString [
	"Answer a string that contains an executable description of my Slots"

	"^self slots ifNotEmpty: [self slots asString] ifEmpty: ['{}']"
	
	^String streamContents: [ :str | | useFull |
		str nextPutAll: '{ '.
		self localSlots do: [:slot |
				str nextPutAll: slot definitionString.
				useFull := slot needsFullDefinition]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				useFull ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]

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
	^self classLayout resolveSlot: aName asSymbol ifFound: foundBlock ifNone: exceptionBlock
]

{ #category : #slots }
ClassDescription >> slotNames [
	^self slots collect: [ :each | each name ]
]

{ #category : #slots }
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
ClassDescription >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	self organization: nil.
]

{ #category : #'accessing tags' }
ClassDescription >> tagWith: aSymbol [
	| package packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we could only model single tag with RPackageTag"
	
	package := self package.
	packageTag := package addClassTag: aSymbol.
	packageTag addClass: self
]

{ #category : #'accessing tags' }
ClassDescription >> tags [
	| packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we only define API to manage them on top of RPackageTag"
	packageTag := self package classTagForClass: self.
	packageTag ifNil: [ ^#() ].
	packageTag isRoot ifTrue: [ ^#() ].
	
	^{packageTag name}
]

{ #category : #'accessing tags' }
ClassDescription >> tagsForMethods [
	"Any method could be tagged with multiple symbols for user purpose. 
	And class contains all method tags which used or not used yet by methods.
	For now we could only implemented it on top of Protocol. 
	It supposed to not include any method tags inherited from Traits 
	which is opposite to current Protocol implementation.
	And extension protocol is not treated as tag"
	| allProtocols |
	allProtocols := self organization protocols reject: [ :each | each name = Protocol unclassified | each isExtensionProtocol ].
		
	^ allProtocols select: [ :each | self isLocalMethodsProtocol: each ] thenCollect: #name
]

{ #category : #'accessing - deprecated parallel hierarchy ' }
ClassDescription >> theMetaClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #classSide instead' transformWith:  '`@receiver theMetaClass' 
						-> '`@receiver classSide'.
	^ self classSide
]

{ #category : #'accessing - deprecated parallel hierarchy ' }
ClassDescription >> theNonMetaClass [
	"This method is deprecated so consider to migrate."
	self deprecated:  'Please use #instanceSide instead' transformWith:  '`@receiver theNonMetaClass' 
						-> '`@receiver instanceSide'.
	^ self instanceSide
]

{ #category : #'accessing method dictionary' }
ClassDescription >> uncategorizedSelectors [
	^ self selectorsInProtocol: Protocol unclassified
]

{ #category : #'accessing tags' }
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
	^false.
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

	(self includesSelector: aSelector)
		ifTrue: [^ self organization categoryOfElement: aSelector]
		ifFalse: [^nil]
]

{ #category : #queries }
ClassDescription >> whichSelectorsAccess: instVarName [
	"Answer a set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."

	^self
		slotNamed: instVarName
		ifFound: [ :slot | 
			self selectors
				select: [ :sel | slot isAccessedIn: self>>sel ] ]
		ifNone: [  #() ]
]

{ #category : #queries }
ClassDescription >> whichSelectorsAssign: instVarName [ 
	self
		deprecated: 'Please use #whichSelectorsStoreInto: instead'
		transformWith: '`@receiver whichSelectorsAssign: `@statements' 
						-> '`@receiver whichSelectorsStoreInto: `@statements'.
	^self whichSelectorsStoreInto: instVarName
]

{ #category : #queries }
ClassDescription >> whichSelectorsRead: instVarName [
	"Answer a Set of selectors whose methods read the argument, 
	instVarName, as a named instance variable."

	^ self
		slotNamed: instVarName
		ifFound:
			[ :slot | self selectors select: [ :sel | slot isReadIn: self >> sel ] ]
		ifNone: [ #() ]
]

{ #category : #queries }
ClassDescription >> whichSelectorsStoreInto: instVarName [
	"Answer a Set of selectors whose methods access the argument, 
	instVarName, as a named instance variable."

	^ self
		slotNamed: instVarName
		ifFound:
			[ :slot | self selectors select: [ :sel | slot isWrittenIn: self >> sel ] ]
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
