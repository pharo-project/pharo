"
Kernel trait for ClassDescription
"
Trait {
	#name : #TClassDescription,
	#category : #'TraitsV2-Compatibility-Traits'
}

{ #category : #compiling }
TClassDescription >> acceptsLoggingOfCompilation [
	"Answer whether the receiver's method submisions and class defintions should be logged to the changes file and to the current change set.  The metaclass follows the rule of the class itself
	Weird name is so that it will come lexically before #compile, so that a clean build can make it through."

	^ true
]

{ #category : #'accessing method dictionary' }
TClassDescription >> addAndClassifySelector: selector withMethod: compiledMethod inProtocol: category notifying: requestor [
	^self explicitRequirement.
]

{ #category : #organization }
TClassDescription >> addCategory: newName before: aCategory [

	^ self organization addCategory: newName before: aCategory 
]

{ #category : #'instance variables' }
TClassDescription >> addInstVarNamed: aString [
	^self explicitRequirement.
]

{ #category : #'accessing tags' }
TClassDescription >> addMethodTag: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose.
	In class we could define what tags could be used for methods"
	self organization addCategory: aSymbol
]

{ #category : #'accessing method dictionary' }
TClassDescription >> addSelector: selector withMethod: compiledMethod notifying: requestor [
	self
		deprecated: 'Please use #addSelector:withMethod: instead'
		transformWith:
			'`@receiver addSelector: `@statements1 withMethod: `@statements2 notifying: `@statements3'
		   -> '`@receiver addSelector: `@statements1 withMethod: `@statements2'.
	^self addSelector: selector withMethod: compiledMethod
]

{ #category : #'accessing method dictionary' }
TClassDescription >> addSelectorSilently: selector withMethod: compiledMethod [
	super addSelectorSilently: selector withMethod: compiledMethod.
	self instanceSide noteAddedSelector: selector meta: self isClassSide.
]

{ #category : #'instance variables' }
TClassDescription >> allInstVarNamesEverywhere [
	"Answer the set of inst var names used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList := OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls instVarNames].
	^ aList asSet

	"BorderedMorph allInstVarNamesEverywhere"
]

{ #category : #'accessing method dictionary' }
TClassDescription >> allProtocolsUpTo: mostGenericClass [
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
TClassDescription >> allSelectorsInProtocol: aName [ 
	"Answer a list of all the methods of the receiver and all its 
	superclasses that are in the protocol named aName"
	
	| aColl |
	aColl := OrderedCollection new.
	self withAllSuperclasses do: [:aClass | 
		aColl addAll: (aClass organization listAtCategoryNamed: aName) ].
	^ aColl asSet asArray sort
]

{ #category : #'pool variable' }
TClassDescription >> allSharedPools [
	"Answer an ordered collection  of the shared pools the receiver shares, including those defined  in the superclasses of the receiver."
	
	^ OrderedCollection new
]

{ #category : #slots }
TClassDescription >> allSlots [
	self explicitRequirement
]

{ #category : #slots }
TClassDescription >> allSlotsEverywhere [
	"Answer the set of slots used by the receiver, all superclasses, and all subclasses"

	| aList |
	aList := OrderedCollection new.
	(self allSuperclasses , self withAllSubclasses asOrderedCollection) do:
		[:cls | aList addAll: cls slots].
	^ aList asSet
]

{ #category : #'organization updating' }
TClassDescription >> applyChangesOfNewTraitCompositionReplacing: oldComposition [
	| changedSelectors oldMethodDict |
	oldMethodDict := self methodDict copy.
	changedSelectors := super applyChangesOfNewTraitCompositionReplacing: oldComposition.
	self noteRecategorizedSelectors: changedSelectors oldComposition: oldComposition.
	self noteChangesFrom: oldMethodDict.
	^ changedSelectors.
]

{ #category : #authors }
TClassDescription >> authors [
	"Returns a bag representing the author frequency based on the latest version of the methods of the receiver."

	^(self methods, self classSide methods) collect: [ :each | each author ] as: Bag
]

{ #category : #organization }
TClassDescription >> basicOrganization [
	^self explicitRequirement.
]

{ #category : #organization }
TClassDescription >> basicOrganization: aClassOrg [
	^self explicitRequirement.
]

{ #category : #'filein/out' }
TClassDescription >> classComment: aString [
	"Store the comment, aString or Text or RemoteString, associated with the class we are orgainzing.  Empty string gets stored only if had a non-empty one before."
	^ self classComment: aString stamp: '<historical>'
]

{ #category : #'filein/out' }
TClassDescription >> classComment: aString stamp: aStamp [
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
TClassDescription >> classCommentBlank [

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
TClassDescription >> classSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	^ self explicitRequirement.
	
]

{ #category : #'instance variables' }
TClassDescription >> classThatDefinesClassVariable: classVarName [
	"Answer the class that defines the given class variable"

	(self classPool includesKey: classVarName asSymbol) ifTrue: [^ self]. 
	^self superclass ifNotNil: [self superclass classThatDefinesClassVariable: classVarName]
]

{ #category : #'instance variables' }
TClassDescription >> classThatDefinesInstanceVariable: instVarName [
	(self instVarNames notNil and: [self instVarNames includes: instVarName asString]) ifTrue: [^ self]. 
	^self superclass ifNotNil: [self superclass classThatDefinesInstanceVariable: instVarName]
]

{ #category : #printing }
TClassDescription >> classVariablesString [
	"Answer a string of my class variable names separated by spaces."

	^String streamContents: [ :stream | 
		self classVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #'accessing class hierarchy' }
TClassDescription >> classesThatImplementAllOf: selectorSet [
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
TClassDescription >> comment [
    "Answer the receiver's comment. (If missing, supply a template) "
    ^self instanceSide organization classComment ifEmpty: [ self classCommentBlank ]
]

{ #category : #'accessing comment' }
TClassDescription >> comment: aStringOrText [
	"Set the receiver's comment to be the argument, aStringOrText."

	self instanceSide classComment: aStringOrText.
]

{ #category : #'accessing comment' }
TClassDescription >> comment: aStringOrText stamp: aStamp [
	"Set the receiver's comment to be the argument, aStringOrText."

	self instanceSide classComment: aStringOrText stamp: aStamp.
]

{ #category : #'accessing class hierarchy' }
TClassDescription >> commentInventory [
	"Answer a string with a count of the classes with and without comments 
	for all the classes in the package of which this class is a member."

	"Morph commentInventory"

	^ self environment organization commentInventory: (self category copyUpTo: $-), '*'
]

{ #category : #'filein/out' }
TClassDescription >> commentStamp: changeStamp [
	self organization commentStamp: changeStamp
]

{ #category : #compiling }
TClassDescription >> compile: code classified: heading [ 
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
TClassDescription >> compile: text classified: category notifying: requestor [
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
TClassDescription >> compile: text classified: category withStamp: changeStamp notifying: requestor [
	^ self
		compile: text
		classified: category
		withStamp: changeStamp
		notifying: requestor
		logSource: self acceptsLoggingOfCompilation
]

{ #category : #compiling }
TClassDescription >> compile: text classified: category withStamp: changeStamp notifying: requestor logSource: logSource [
 
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
		inProtocol: category 
		notifying: requestor.
			
	self instanceSide 
		noteCompilationOfMethod: method 
		meta: self isClassSide.

	^ selector
]

{ #category : #compiling }
TClassDescription >> compile: code notifying: requestor [ 
	"Refer to the comment in Behavior|compile:notifying:." 

	^self compile: code
		 classified: Protocol unclassified
		 notifying: requestor
]

{ #category : #compiling }
TClassDescription >> compileSilently: code classified: category [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ self compileSilently: code classified: category notifying: nil.
]

{ #category : #compiling }
TClassDescription >> compileSilently: code classified: category notifying: requestor [
	"Compile the code and classify the resulting method in the given category, leaving no trail in the system log, nor in any change set, nor in the 'recent submissions' list. This should only be used when you know for sure that the compilation will succeed."

	^ SystemAnnouncer uniqueInstance 
		suspendAllWhile: [self compile: code classified: category notifying: requestor].
]

{ #category : #copying }
TClassDescription >> copy: sel from: class [ 
	"Install the method associated with the first argument, sel, a message 
	selector, found in the method dictionary of the second argument, class, 
	as one of the receiver's methods. Classify the message under -As yet not 
	classified-."

	self copy: sel
		from: class
		classified: nil
]

{ #category : #copying }
TClassDescription >> copy: sel from: class classified: cat [ 
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
TClassDescription >> copyAll: selArray from: class [ 
	"Install all the methods found in the method dictionary of the second 
	argument, class, as the receiver's methods. Classify the messages under 
	-As yet not classified-."

	self copyAll: selArray
		from: class
		classified: nil
]

{ #category : #copying }
TClassDescription >> copyAll: selArray from: class classified: cat [ 
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
TClassDescription >> copyAllCategoriesFrom: aClass [ 
	"Specify that the categories of messages for the receiver include all of 
	those found in the class, aClass. Install each of the messages found in 
	these categories into the method dictionary of the receiver, classified 
	under the appropriate categories."

	aClass organization categories do: [:cat | self copyCategory: cat from: aClass]
]

{ #category : #copying }
TClassDescription >> copyCategory: cat from: class [ 
	"Specify that one of the categories of messages for the receiver is cat, as 
	found in the class, class. Copy each message found in this category."

	self copyCategory: cat
		from: class
		classified: cat
]

{ #category : #copying }
TClassDescription >> copyCategory: cat from: aClass classified: newCat [ 
	"Specify that one of the categories of messages for the receiver is the 
	third argument, newCat. Copy each message found in the category cat in 
	class aClass into this new category."

	self copyAll: (aClass organization listAtCategoryNamed: cat)
		from: aClass
		classified: newCat
]

{ #category : #copying }
TClassDescription >> copyMethodDictionaryFrom: donorClass [
	"Copy the method dictionary of the donor class over to the receiver"

	self methodDict: donorClass copyOfMethodDictionary.
	self organization: donorClass organization deepCopy.
]

{ #category : #slots }
TClassDescription >> definesSlot: aSlot [
	"Return true whether the receiver defines an instance variable named aString"
	
	^ self slots identityIncludes: aSlot
]

{ #category : #slots }
TClassDescription >> definesSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self slotNames includes: aString
]

{ #category : #'filein/out' }
TClassDescription >> definition [
	^self explicitRequirement.
]

{ #category : #accessing }
TClassDescription >> definitionForNautilus [

	^ self definition
]

{ #category : #accessing }
TClassDescription >> definitionForNautilusWithSlots [

	^ self definition
]

{ #category : #private }
TClassDescription >> errorCategoryName [
	self error: 'Category name must be a String'
]

{ #category : #'instance variables' }
TClassDescription >> forceNewFrom: anArray [
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
TClassDescription >> hasClassSide [
	^self subclassResponsibility.
]

{ #category : #'accessing comment' }
TClassDescription >> hasComment [
	"return whether this class truly has a comment other than the default"
	| org |
	org := self instanceSide organization.
	^org classComment isEmptyOrNil not
]

{ #category : #'instance variables' }
TClassDescription >> hasInstVarNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self instVarNames includes: aString
]

{ #category : #'pool variable' }
TClassDescription >> hasSharedPools [
	"Only a class may have shared pools"
	^ false
]

{ #category : #slots }
TClassDescription >> hasSlot: aSlot [
	"Return true whether the receivers hierarchy defines an instance variable named aString."
	
	^ self allSlots identityIncludes: aSlot
]

{ #category : #slots }
TClassDescription >> hasSlotNamed: aString [
	"Return true whether the receiver defines an instance variable named aString."
	
	^ self allSlots anySatisfy: [:slot | slot name = aString]
]

{ #category : #'instance variables' }
TClassDescription >> instVarIndexFor: instVarName [
	"Answer the index of the named instance variable."

	^self instVarIndexFor: instVarName ifAbsent: 0
]

{ #category : #'instance variables' }
TClassDescription >> instVarIndexFor: instVarName ifAbsent: aBlock [
	"Answer the index of the named instance variable."

	| index |
	index := self classLayout instVarIndexFor: instVarName ifAbsent: 0.
	index = 0 ifTrue: 
		[^self superclass
			ifNil: [aBlock value]
			ifNotNil: [self superclass instVarIndexFor: instVarName ifAbsent: aBlock]].
	^self superclass
		ifNil: [index]
		ifNotNil: [index + self superclass instSize]
]

{ #category : #private }
TClassDescription >> instVarMappingFrom: oldClass [
	"Return the mapping from instVars of oldClass to new class that is used for converting old instances of oldClass."
	| oldInstVarNames |
	oldInstVarNames := oldClass allInstVarNames.
	^self allInstVarNames 
			collect: [:instVarName | oldInstVarNames indexOf: instVarName].
]

{ #category : #'instance variables' }
TClassDescription >> instVarNameForIndex: index [
	"Answer the named instance variable with index index or nil if none."

	| superInstSize |
	index > self instSize ifTrue: [^nil].
	superInstSize := self superclass ifNil: [0] ifNotNil: [self superclass instSize].
	index > superInstSize ifTrue:
		[^self instVarNames at: index - superInstSize].
	self superclass ifNil: [^nil].
	^self superclass instVarNameForIndex: index

	"(Object allSubclasses select:
		[:cls| cls instSize > cls superclass instSize and: [cls subclasses isEmpty and: [cls superclass instSize > 0]]]) collect:
			[:cls| (1 to: cls instSize) collect: [:i| cls instVarNameForIndex: i]]"
]

{ #category : #'instance variables' }
TClassDescription >> instVarNames [
	"Answer an Array of the receiver's instance variable names."

	^self classLayout instVarNames
]

{ #category : #compiling }
TClassDescription >> instVarNamesAndOffsetsDo: aBinaryBlock [
	"This is part of the interface between the compiler and a class's instance or field names.
	 The class should enumerate aBinaryBlock with the instance variable name strings and
	 their integer offsets.  The order is important. Names evaluated later will override the
	 same names occurring earlier."

	| superInstSize |
	(superInstSize := self superclass notNil ifTrue: [self superclass instSize] ifFalse: [0]) > 0 ifTrue:
		[self superclass instVarNamesAndOffsetsDo: aBinaryBlock].
	1 to: self instSize - superInstSize do:
		[:i| aBinaryBlock value: (self instVarNames at: i) value: i + superInstSize]
]

{ #category : #'accessing parallel hierarchy' }
TClassDescription >> instanceSide [
	"Return the class of the couple class/metaclass. Useful to avoid explicit test."
	^ self explicitRequirement
]

{ #category : #accessing }
TClassDescription >> instanceVariables [
	^self explicitRequirement.
]

{ #category : #printing }
TClassDescription >> instanceVariablesString [
	"Answer a string of my instance variable names separated by spaces."

	^String streamContents: [ :stream |
		self instVarNames 
			do: [ :each | stream nextPutAll: each ]
			separatedBy: [ stream space ] ]
]

{ #category : #'accessing parallel hierarchy' }
TClassDescription >> isClassSide [
	^ self == self classSide
]

{ #category : #'accessing parallel hierarchy' }
TClassDescription >> isInstanceSide [
	^self isClassSide not
]

{ #category : #testing }
TClassDescription >> isLocalMethodsProtocol: aProtocol [
	aProtocol methods ifEmpty: [ ^true ].
	
	^aProtocol methods anySatisfy: [ :each | self isLocalSelector: each]
]

{ #category : #'accessing parallel hierarchy' }
TClassDescription >> isMeta [
	^ self isClassSide
]

{ #category : #testing }
TClassDescription >> isTaggedWith: aSymbol [
	^self tags includes: aSymbol 
]

{ #category : #private }
TClassDescription >> linesOfCode [ 
	"An approximate measure of lines of code.
	Includes comments, but excludes blank lines."

	| lines |
	lines := self localMethods inject: 0 into: [ :sum :each | sum + each linesOfCode ].
	^ self isClassSide 
		ifTrue: [ lines ]
		ifFalse: [ lines + self classSide linesOfCode ]
]

{ #category : #'accessing tags' }
TClassDescription >> methodsTaggedWith: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose. 
	And class contains all method tags which used or not used yet by methods.
	This method select all methods marked by given tag.
	Tags are not inherited from traits. So we only select local methods"
	
	^self localMethods select: [ :each | each isTaggedWith: aSymbol ]
]

{ #category : #private }
TClassDescription >> newInstanceFrom: oldInstance variable: variable size: instSize [
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
					newSlot wantsInitalization ifTrue: [ self initializeSlots: new ].
					value := oldSlot read: oldInstance.
					newSlot write: value to: new ] ].
	variable
		ifTrue: [ 1 to: oldInstance basicSize do: [ :offset | new basicAt: offset put: (oldInstance basicAt: offset) ] ].
	^ new
]

{ #category : #private }
TClassDescription >> newInstanceFrom: oldInstance variable: variable size: instSize map: map [
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
TClassDescription >> noteAddedSelector: aSelector meta: isMeta [
	"A hook allowing some classes to react to adding of certain selectors"
]

{ #category : #'organization updating' }
TClassDescription >> noteChangesFrom: oldMethodDict [
	^self explicitRequirement.
	
]

{ #category : #compiling }
TClassDescription >> noteCompilationOf: aSelector meta: isMeta [
	"A hook allowing some classes to react to recompilation of certain selectors"
]

{ #category : #compiling }
TClassDescription >> noteCompilationOfMethod: aCompiledMethod meta: isMeta [
	self noteCompilationOf: aCompiledMethod selector meta: isMeta
]

{ #category : #'organization updating' }
TClassDescription >> noteMethodAdded: aMethod [
	SystemAnnouncer uniqueInstance
		methodAdded: aMethod
]

{ #category : #'organization updating' }
TClassDescription >> noteMethodChanged: oldMethod to: newMethod [
	self flag: #todo. "oldProtocol is not set properly here"
	SystemAnnouncer uniqueInstance methodChangedFrom: oldMethod to: newMethod oldProtocol: Protocol unclassified
]

{ #category : #'organization updating' }
TClassDescription >> noteMethodRemoved: oldMethod [
	self flag: #todo. "protocol and origin are not set properly here"
	SystemAnnouncer uniqueInstance methodRemoved: oldMethod protocol: oldMethod protocol origin: oldMethod origin
]

{ #category : #'organization updating' }
TClassDescription >> noteRecategorizedSelector: aSymbol from: oldCategoryOrNil to: newCategoryOrNil [
	| changedCategories |
	changedCategories := self updateOrganizationSelector: aSymbol oldCategory: oldCategoryOrNil newCategory: newCategoryOrNil.
	changedCategories do: [:each |
		(self organization isEmptyCategoryNamed: each) ifTrue: [self organization removeCategory: each]]
]

{ #category : #'organization updating' }
TClassDescription >> noteRecategorizedSelectors: aCollection oldComposition: aTraitComposition [
	
	aCollection do: [:each | | oldCategory newCategory | 
		oldCategory := self organization categoryOfElement: each.
		newCategory := (self traitComposition methodDescriptionForSelector: each) effectiveMethodCategory.
		self noteRecategorizedSelector: each from: oldCategory to: newCategory]
]

{ #category : #'organization updating' }
TClassDescription >> notifyOfRecategorizedSelector: selector from: oldCategory to: newCategory [
	"If compiled method is not there, it meens it has been removed, not recategorized... so I skip 
	 the method recategorized announce"
	(self compiledMethodAt: selector ifAbsent: [ nil ]) 
		ifNotNil: [ :method | 
			SystemAnnouncer uniqueInstance 
				methodRecategorized: method
				oldProtocol: oldCategory ].
	SystemAnnouncer uniqueInstance suspendAllWhile: [
		self 
			notifyUsersOfRecategorizedSelector: selector 
			from: oldCategory 
			to: newCategory ]
]

{ #category : #private }
TClassDescription >> notifyRepackage: selector method: compiledMethod oldProtocol: oldProtocol newProtocol: newProtocol [
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

{ #category : #'users notification' }
TClassDescription >> notifyUsersOfChangedSelectors: aCollection [
	self traitUsers do: [:each |
		each noteChangedSelectors: aCollection]
]

{ #category : #'users notification' }
TClassDescription >> notifyUsersOfRecategorizedSelector: element from: oldCategory to: newCategory [
	self traitUsers do: [:each |
		each noteRecategorizedSelector: element from: oldCategory to: newCategory]
]

{ #category : #private }
TClassDescription >> numberOfMethods [
	"count all methods that are local (not comming from a trait)"
	| num |
	num := self localMethods size.
	^ self isClassSide  
		ifTrue: [ num ]
		ifFalse: [ num + self classSide numberOfMethods ] 
]

{ #category : #'initialize-release' }
TClassDescription >> obsolete [
	"Make the receiver obsolete."
	self superclass removeSubclass: self.
	self organization: nil.
	super obsolete.
]

{ #category : #organization }
TClassDescription >> organization [
	"Answer the instance of ClassOrganizer that represents the organization 
	of the messages of the receiver."

	self basicOrganization
		ifNil: [ self basicOrganization: (self isTrait
						ifTrue: [ ClassOrganization new ]
						ifFalse: [ ClassOrganization forClass: self ]) ].
	^self basicOrganization setSubject: self.	"Making sure that subject is set correctly. It should not be necessary."
	
	
]

{ #category : #organization }
TClassDescription >> organization: aClassOrg [
	"Install an instance of ClassOrganizer that represents the organization of the messages of the receiver."

	aClassOrg ifNotNil: [aClassOrg setSubject: self].
	self basicOrganization: aClassOrg.
]

{ #category : #printing }
TClassDescription >> printOn: aStream [ 
	aStream nextPutAll: self name
]

{ #category : #'accessing class hierarchy' }
TClassDescription >> printSubclassesOn: aStream level: level [ 

		self printSubclassesOn: aStream level: level filter: nil
]

{ #category : #'accessing class hierarchy' }
TClassDescription >> printSubclassesOn: aStream level: level filter: filterCollection [
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
TClassDescription >> reformatAll [
	"Reformat all methods in this class"
	self methods do: [:method | method reformat]
]

{ #category : #'instance variables' }
TClassDescription >> removeInstVarNamed: aString [ 
	"Remove the argument, aString, as one of the receiver's instance 
	variables. Create an error notification if the argument is not found."

	^self subclassResponsibility
]

{ #category : #'accessing tags' }
TClassDescription >> removeMethodTag: aSymbol [
	"Any method could be tagged with multiple symbols for user purpose.
	In class we could define what tags could be used for methods.
	Here we could remove what we not need"
	self organization removeCategory: aSymbol
]

{ #category : #'accessing method dictionary' }
TClassDescription >> removeProtocol: aString [ 
	"Remove each of the messages categorized under aString in the method 
	dictionary of the receiver. Then remove the category aString."
	| categoryName |
	categoryName := aString asSymbol.
	(self organization listAtCategoryNamed: categoryName) do:
		[:sel | self removeSelector: sel].
	self organization removeCategory: categoryName
]

{ #category : #'accessing method dictionary' }
TClassDescription >> removeSelector: selector [ 
	"Remove the message whose selector is given from the method 
	dictionary of the receiver, if it is there. Answer nil otherwise."
	
	| priorMethod priorProtocol origin | 
	priorMethod := self compiledMethodAt: selector ifAbsent: [^ nil].
	origin := priorMethod origin.
	priorProtocol := self whichCategoryIncludesSelector: selector.
	super removeSelector: selector.
	SystemAnnouncer uniqueInstance 
		suspendAllWhile: [self updateOrganizationSelector: selector oldCategory: priorProtocol newCategory: nil].
	SystemAnnouncer uniqueInstance methodRemoved: priorMethod protocol: priorProtocol origin: origin
]

{ #category : #organization }
TClassDescription >> reorganize [
	"During fileIn, !Rectangle reorganize! allows Rectangle to seize control and treat the next chunk as its organization.  See the transfer of control where ReadWriteStream fileIn calls scanFrom:"

	^self organization
]

{ #category : #'accessing method dictionary' }
TClassDescription >> selectorsInCategory: aName [ 
	"Answer a list of the selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: (self organization listAtCategoryNamed: aName).
	^ aColl asArray sort
]

{ #category : #'accessing method dictionary' }
TClassDescription >> selectorsInProtocol: aName [ 
	"Answer a list of the selectors of the receiver that are in category named aName"
	
	| aColl |
	aColl := Set withAll: (self organization listAtCategoryNamed: aName).
	^ aColl asArray sort
]

{ #category : #'pool variable' }
TClassDescription >> sharedPoolOfVarNamed: aString [
	"Only classes may have shared pools"
	
	^ nil
]

{ #category : #printing }
TClassDescription >> sharedPoolsString [
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
TClassDescription >> slotDefinitionString [
	"Answer a string that contains an executable description of my Slots"

	"^self slots ifNotEmpty: [self slots asString] ifEmpty: ['{}']"
	
	^String streamContents: [ :str | | special |
		str nextPutAll: '{ '.
		self slots do: [:slot |
				str nextPutAll: slot definitionString.
				special := slot isSpecial]				
			separatedBy: [ 
				str nextPutAll: '. '.  
				special ifTrue: [ str cr;tab;tab;tab;tab ]].
		str nextPutAll: ' }'. ]

]

{ #category : #slots }
TClassDescription >> slotNamed: aName [
	self explicitRequirement
]

{ #category : #slots }
TClassDescription >> slotNamed: aName ifFound: foundBlock [
	self explicitRequirement
]

{ #category : #slots }
TClassDescription >> slotNamed: aName ifFound: foundBlock ifNone: exceptionBlock [
	self explicitRequirement
]

{ #category : #slots }
TClassDescription >> slotNames [
	^self slots collect: [ :each | each name ]
]

{ #category : #slots }
TClassDescription >> slots [
	self explicitRequirement
]

{ #category : #private }
TClassDescription >> spaceUsed [
	^super spaceUsed + (self hasClassSide
		ifTrue: [self classSide spaceUsed] 
		ifFalse: [0])
]

{ #category : #printing }
TClassDescription >> storeOn: aStream [
	"Classes and Metaclasses have global names."

	aStream nextPutAll: self name
]

{ #category : #'accessing class hierarchy' }
TClassDescription >> subclassesDo: aBlock [
	"Evaluate the argument, aBlock, for each of the receiver's immediate subclasses."
	^self subclasses do: aBlock
]

{ #category : #'initialize-release' }
TClassDescription >> superclass: aClass methodDictionary: mDict format: fmt [
	"Basic initialization of the receiver"
	super superclass: aClass methodDictionary: mDict format: fmt.
	self organization: nil.
]

{ #category : #'accessing tags' }
TClassDescription >> tagWith: aSymbol [
	| package packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we could only model single tag with RPackageTag"
	
	package := self package.
	packageTag := package addClassTag: aSymbol.
	packageTag addClass: self
]

{ #category : #'accessing tags' }
TClassDescription >> tags [
	| packageTag |
	"Any class or trait could be tagged by multiple symbols for user purpose.
	For now we only define API to manage them on top of RPackageTag"
	packageTag := self package classTagForClass: self.
	packageTag ifNil: [ ^#() ].
	packageTag isRoot ifTrue: [ ^#() ].
	
	^{packageTag name}
]

{ #category : #'accessing tags' }
TClassDescription >> tagsForMethods [
	"Any method could be tagged with multiple symbols for user purpose. 
	And class contains all method tags which used or not used yet by methods.
	For now we could only implemented it on top of Protocol. 
	It supposed to not include any method tags inherited from Traits 
	which is opposite to current Protocol implementation.
	And extension protocol is not treated as tag"
	| allProtocols tags |
	allProtocols := self organization protocols 
		reject: [ :each | each name = Protocol unclassified | each isExtensionProtocol ].
		
	tags := OrderedCollection new.
	allProtocols do: [ :each | 
		(self isLocalMethodsProtocol: each) ifTrue: [ tags add: each name ]].

	^tags
]

{ #category : #'accessing - deprecated parallel hierarchy' }
TClassDescription >> theMetaClass [
	^self classSide
]

{ #category : #'accessing - deprecated parallel hierarchy' }
TClassDescription >> theNonMetaClass [
	^self instanceSide
]

{ #category : #'accessing method dictionary' }
TClassDescription >> uncategorizedSelectors [
	^ self selectorsInProtocol: Protocol unclassified
]

{ #category : #'accessing tags' }
TClassDescription >> untagFrom: aSymbol [
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

{ #category : #'initialize-release' }
TClassDescription >> updateInstances: oldInstances from: oldClass isMeta: isMeta [
	"Recreate any existing instances of the argument, oldClass, as instances of the receiver,
	 which is a newly changed class. Permute variables as necessary, and forward old instances
	 to new instances.  Answer nil to defeat old clients that expect an array of old instances.
	 The old behaviour, which necessitated a global GC, exchanged identities and answered
	 the old instances.  But no clients used the result.  This way we avoid the unnecessary GC,"

	| variable instSize newInstances |
	oldInstances isEmpty
		ifTrue: [ ^ nil ].	"no instances to convert"
	isMeta ifTrue:
		[(oldInstances size = 1
		  and: [self soleInstance class == self
				or: [self soleInstance class == oldClass]]) ifFalse:
			[^self error: 'Metaclasses can only have one instance']].
	variable := self isVariable.
	instSize := self instSize.
	newInstances := Array new: oldInstances size.
	1 to: oldInstances size do: [ :i | newInstances at: i put: (self newInstanceFrom: (oldInstances at: i) variable: variable size: instSize) ].	"Now perform a bulk mutation of old instances into new ones"
	oldInstances elementsForwardIdentityTo: newInstances.
	^ nil
]

{ #category : #'initialize-release' }
TClassDescription >> updateInstancesFrom: oldClass [
	"Recreate any existing instances of the argument, oldClass, as instances of 
	the receiver, which is a newly changed class. Permute variables as 
	necessary. Return the array of old instances (none of which should be
	pointed to legally by anyone but the array)."
	"ar 7/15/1999: The updating below is possibly dangerous. If there are any
	contexts having an old instance as receiver it might crash the system if
	the new receiver in which the context is executed has a different layout.
	See bottom below for a simple example:"
	| oldInstances |
	oldInstances := oldClass allInstances asArray.
	oldInstances := self updateInstances: oldInstances from: oldClass isMeta: self isClassSide.
	^nil

"	| crashingBlock class |
	class := Object subclass: #CrashTestDummy
		instanceVariableNames: 'instVar'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	class compile:'instVar: value instVar := value'.
	class compile:'crashingBlock ^[instVar]'.
	crashingBlock := (class new) instVar: 42; crashingBlock.
	Object subclass: #CrashTestDummy
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Crash-Test'.
	crashingBlock.
	crashingBlock value.
	"

]

{ #category : #'organization updating' }
TClassDescription >> updateOrganizationDescription: each oldCategory: newCategoryOrNil newCategory: oldCategoryOrNil changed: changedCategories [
	| sel effectiveCategory currentCategory |
	
	sel := each selector.
	(self includesLocalSelector: sel) ifTrue: [ ^ self ].
	
	currentCategory := self organization categoryOfElement: sel.
	effectiveCategory := each 
		effectiveMethodCategoryCurrent: currentCategory 
		new: newCategoryOrNil.
	
	effectiveCategory 
		ifNil: [ 
			currentCategory ifNotNil: [ changedCategories add: currentCategory ].
			self organization removeElement: sel.
			^ self ].
		
	(currentCategory isNil 
		or: [ currentCategory == Protocol ambiguous 
		or: [ currentCategory == oldCategoryOrNil ] ])
	ifFalse: [ ^ self ].
	
	(currentCategory ~~ effectiveCategory)
		ifTrue: [ 
			currentCategory ifNotNil: [ changedCategories add: currentCategory ].
			self organization classify: sel under: effectiveCategory suppressIfDefault: false ]
]

{ #category : #'organization updating' }
TClassDescription >> updateOrganizationDescription: aTraitMethodDescription oldCategory: oldCategoryOrNil newCategory: newCategoryOrNil changedCategories: changedCategories [
	| sel effectiveCategory currentCategory |
	sel := aTraitMethodDescription selector.
	
	(self includesLocalSelector: sel) ifTrue: [ ^ self ].
	 
	currentCategory := self organization categoryOfElement: sel.
	effectiveCategory := aTraitMethodDescription effectiveMethodCategoryCurrent: currentCategory new: newCategoryOrNil.
	
	effectiveCategory ifNil: [ 
		currentCategory ifNotNil: [ changedCategories add: currentCategory ].
		^ self organization removeElement: sel ].
		
	((currentCategory isNil 
		or: [ 
				currentCategory == Protocol ambiguous 
				or: [ currentCategory == oldCategoryOrNil ] ])
		and: [ currentCategory ~~ effectiveCategory ])
		ifTrue: [ 
			currentCategory ifNotNil: [ changedCategories add: currentCategory ].
			self organization classify: sel under: effectiveCategory suppressIfDefault: false ]
]

{ #category : #'organization updating' }
TClassDescription >> updateOrganizationSelector: aSymbol oldCategory: oldCategoryOrNil newCategory: newCategoryOrNil [
	^self explicitRequirement.
]

{ #category : #'pool variable' }
TClassDescription >> usesLocalPoolVarNamed: aString [
	^false.
]

{ #category : #'pool variable' }
TClassDescription >> usesPoolVarNamed: aString [
	"Only classes may use a pool variable named: aString"
	
	^ false
]

{ #category : #slots }
TClassDescription >> usesSpecialClassVariables [

	^self classVariables anySatisfy: [ :each | each isSpecial ]
]

{ #category : #slots }
TClassDescription >> usesSpecialSlot [
	"return true if we define something else than InstanceVariableSlots"
	^self slots anySatisfy: [ :each | each isSpecial ]
]

{ #category : #slots }
TClassDescription >> usesSpecialVariables [
    "return true if we define something else than InstanceVariableSlots"
    ^(self usesSpecialSlot 
        or: [ self class usesSpecialSlot ])
        or: [self usesSpecialClassVariables ]
]

{ #category : #compiling }
TClassDescription >> wantsChangeSetLogging [
	"Answer whether code submitted for the receiver should be remembered by the changeSet mechanism."

	^ true
]

{ #category : #compiling }
TClassDescription >> wantsRecompilationProgressReported [
	"Answer whether the receiver would like progress of its recompilation reported interactively to the user."

	^ true
]

{ #category : #organization }
TClassDescription >> whichCategoryIncludesSelector: aSelector [ 
	"Answer the category of the argument, aSelector, in the organization of 
	the receiver, or answer nil if the receiver does not inlcude this selector."

	(self includesSelector: aSelector)
		ifTrue: [^ self organization categoryOfElement: aSelector]
		ifFalse: [^nil]
]

{ #category : #organization }
TClassDescription >> zapOrganization [
	"Remove the organization of this class by message categories.
	This is typically done to save space in small systems.  Classes and methods
	created or filed in subsequently will, nonetheless, be organized"

	self organization: nil.
	self isClassSide ifFalse: [self classSide zapOrganization]
]
