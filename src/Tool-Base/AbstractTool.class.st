"
I'm an abstract class grouping generic methods for managing packages/classes/groups/methods from a browser
"
Class {
	#name : #AbstractTool,
	#superclass : #Model,
	#category : #'Tool-Base'
}

{ #category : #private }
AbstractTool class >> defineClassTemplate [ 
	^ '{1} subclass: #{2}
		instanceVariableNames: '''' 
		classVariableNames: ''''
		package: ''{3}'''
]

{ #category : #private }
AbstractTool class >> defineTraitTemplate [ 
	^ 'Trait named: #{1}
	uses: {2}
	package: ''{3}'''
]

{ #category : #'world menu' }
AbstractTool class >> menuCommandOn: aBuilder [
	<worldMenu>
	(aBuilder item: #Tools)
		order: 1.0;
		target: self;
		help: 'Set of tools to get a better Pharo experience.';
		iconName: #toolsIcon
]

{ #category : #private }
AbstractTool class >> protocolSuggestionsFor: aClass [

	| allExistingProtocols interestingProtocols reject |
	reject := Set new.
	reject
		addAll: aClass organization allCategories;
		add: AllProtocol defaultName;
		add: Protocol nullCategory;
		add: Protocol unclassified.
	allExistingProtocols := SystemNavigation default allExistingProtocolsFor: aClass isMeta not.
	
	interestingProtocols := allExistingProtocols reject: [ :e | reject includes: e ].
	^ interestingProtocols asOrderedCollection sort: [ :a :b | a asLowercase < b asLowercase ].
]

{ #category : #private }
AbstractTool class >> requestProtocolNameFor: aClass initialAnswer: aString [
	| ui |
	ui := ListDialogWindow new
		getList: [ :r | (self protocolSuggestionsFor: aClass) select: [ :e | r search: e ] ];
		displayBlock: [ :e | e ];
		initialAnswer: aString;
		acceptNewEntry: true;
		title: 'New Protocol Name' ;
		yourself.
	^ ui chooseFromOwner: World
]

{ #category : #'world menu' }
AbstractTool class >> theme [
	^ Smalltalk ui theme 
]

{ #category : #class }
AbstractTool >> addCategoryIn: aClass before: aCategory [
	"Present a choice of categories or prompt for a new category name 
	and add it before the current selection, or at the end if no current selection"

	| newName |
	newName := self class requestProtocolNameFor: aClass initialAnswer: ''.
	(newName isEmptyOrNil) ifTrue: [ ^ nil ].
	newName := newName asSymbol.

	aClass organization
		addCategory: newName
		before: aCategory.
	
	^ newName
]

{ #category : #class }
AbstractTool >> addClassIn: aPackage [
	" Add a new class in the provided package. A dialog will be opened to let the user set the class definition "

	| classSymbol systemCategory proposedSuperclass classDefinition newClass  |
	classSymbol := #NameOfSubclass.
	systemCategory := aPackage
		ifNotNil: [ aPackage categoryName ]
		ifNil: [ 'Unknown' ].
	proposedSuperclass := self selectedClasses in: [ :classes |
		classes size == 1
			ifFalse: [ Object ]
			ifTrue: [ classes first ]].
	classDefinition := self class defineClassTemplate 
		format: { proposedSuperclass. classSymbol. systemCategory }.
	classDefinition := UIManager default 
		multiLineRequest: 'Define class definition:'
		initialAnswer: classDefinition
		answerHeight: 250.
	classDefinition isEmptyOrNil 
		ifTrue: [ ^ self ].
		
	newClass := self compileANewClassFrom: classDefinition notifying: nil startingFrom: nil.
	newClass ifNotNil: [self browseClass: newClass].
]

{ #category : #package }
AbstractTool >> addPackageBasedOn: aPackage [

	| name initialAnswer package |
	initialAnswer := aPackage ifNil: [''] ifNotNil: [:p | p name].
	name := UIManager default request: 'Name of the new package:' initialAnswer: initialAnswer title: 'Create a new package'.
	(self isValidPackageName: name)
		ifTrue: [ package := self browsedEnvironment packageOrganizer createPackageNamed: name ]
		ifFalse: [ self alertInvalidPackageName:name ].
	^ package
]

{ #category : #class }
AbstractTool >> addTraitIn: aPackage [
	" add a new class in the provided package. A dialog will be opened to let the user set the class definition "
	|  systemCategory classDefinition newTrait |
	
	systemCategory := aPackage
		ifNotNil: [ aPackage categoryName ]
		ifNil: [ 'Unknown' ].
	classDefinition := self class defineTraitTemplate 
		format: { #TNameOfTrait. '{}'. systemCategory }.
	classDefinition := UIManager default 
		multiLineRequest: 'Define trait definition:'
		initialAnswer: classDefinition
		answerHeight: 250.
	(classDefinition isNil or: [ classDefinition isEmpty ])
		ifTrue: [ ^ self ].
	self class evaluate: classDefinition.
	
	newTrait := self class evaluate: classDefinition.
	self browseClass: newTrait.
]

{ #category : #package }
AbstractTool >> alertInvalidPackageName:name [
	| errorMessage |

	(name isNil or: [ name isEmpty ]) ifTrue: [ ^ self ]. "User cancelled the action"
	
	errorMessage := String streamContents: [:stream |
		stream << name << ' is not a valid package name.
A package name can not begin with a *' ].
	
	UIManager default inform: errorMessage
]

{ #category : #class }
AbstractTool >> browseClassRefsOf: aClass [

	| class |
	aClass ifNil: [ ^self ].
	class := aClass theNonMetaClass ifNil: [^self].
	class isTrait 
		ifTrue: [self systemNavigation browseAllUsersOfTrait: class]
		ifFalse: [self systemNavigation browseAllCallsOnClass: class]

]

{ #category : #class }
AbstractTool >> browseClassVarRefsOf: aClass [

	| cls |
	cls := aClass theNonMetaClass.
	(cls notNil and: [cls isTrait not])
		ifTrue: [self systemNavigation browseClassVarRefs: cls]
]

{ #category : #class }
AbstractTool >> browseInstVarDefsOf: aClass [

	(aClass notNil and: [aClass isTrait not])
		ifTrue: [self systemNavigation browseInstVarDefs: aClass]
]

{ #category : #class }
AbstractTool >> browseInstVarRefsOf: aClass [

	(aClass notNil and: [aClass isTrait not])
		ifTrue: [self systemNavigation browseInstVarRefs: aClass]
]

{ #category : #method }
AbstractTool >> browseMessagesFrom: aSelector [
	" badly named, it browses implementors "

	self getSelectorAndSendQuery: #browseAllImplementorsOf: to: self systemNavigation with: {} selector: aSelector
]

{ #category : #method }
AbstractTool >> browseSendersOfMessagesFrom: aSelector [
	"Present a menu of the currently selected message, as well as all messages sent by it.  Open a message set browser of all senders of the selector chosen."

	self getSelectorAndSendQuery: #browseAllSendersOf: to: self systemNavigation with: {} selector: aSelector
]

{ #category : #class }
AbstractTool >> browseUnusedMethodsOf: aClass [
	| classes unsent messageList |
	
	aClass
		ifNil: [^ self].
		
	classes := Array with: aClass with: aClass class.
	unsent := Set new.
	classes
		do: [:c | unsent addAll: c selectors ].
	unsent := self systemNavigation allUnsentMessagesIn: unsent.
	messageList := OrderedCollection new.
	classes
		do: [:c | (c selectors
				select: [:s | unsent includes: s]) asSortedCollection
				do: [:sel | messageList add: ((c compiledMethodAt: sel) methodReference) ]].
	self systemNavigation browseMessageList: messageList name: 'Unsent Methods in ' , aClass name
]

{ #category : #method }
AbstractTool >> browseVersionsFrom: aMethod [
	"Create and schedule a Versions Browser, showing all versions of the 
	currently selected message. Answer the browser or nil."

	aMethod selector
		ifNil: [ self inform: 'Sorry, only actual methods have retrievable versions.'. ^nil ]
		ifNotNil: [:selector |
			Smalltalk tools versionBrowser browseVersionsForMethod: aMethod ]
				
]

{ #category : #method }
AbstractTool >> categorizeMethods: aCollection of: aClass from: aCategory [
	"Present a choice of categories or prompt for a new category name and add it before the current selection, or at the end if no current selection"
	
	| labels lines newName |
	aCollection ifEmpty: [^ self].
	aClass ifNil: [^self].	
	labels := OrderedCollection new.
	labels
		addAll: aClass organization allCategories sorted;
		add: Protocol unclassified.
	lines := OrderedCollection new.
	lines add: labels size -1.
	newName := UIManager default chooseOrRequestFrom: labels lines: lines title: ('Change Protocol (current: ', aCategory, ')').
	newName ifNil: [ ^self ].
	newName := newName asSymbol.
	aCollection do: [:item | 
		item methodClass organization
					classify: item selector
					under: newName 
					suppressIfDefault: true ].
]

{ #category : #class }
AbstractTool >> compileANewClassFrom: aString notifying: aController startingFrom: aClass [
	
	" Copied from Browser "
	
	"The receiver's textual content is a request to define a new class. The
	source code is defString. If any errors occur in compilation, notify
	aController."
	| oldClass class newClassName defTokens keywdIx envt |
	oldClass := aClass.
	defTokens := aString findTokens: Character separators.
	
	((defTokens first = 'Trait' and: [defTokens second = 'named:'])
		or: [defTokens second = 'classTrait'])
		ifTrue: [^ self defineTrait: aString notifying: aController ].
		
	keywdIx := defTokens findFirst: [:x | x beginsWith: 'category'].
	envt := Smalltalk globals.
	keywdIx := defTokens findFirst: [:x | '*subclass*' match: x].
	newClassName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldClass isNil or: [oldClass theNonMetaClass name asString ~= newClassName])
		and: [envt includesKey: newClassName asSymbol]) ifTrue:
			["Attempting to define new class over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newClassName , ' is an existing class in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newClassName size))
				ifFalse: [^ nil]].
	"ar 8/29/1999: Use oldClass superclass for defining oldClass
	since oldClass superclass knows the definerClass of oldClass."
	oldClass ifNotNil:[oldClass := oldClass superclass].
	class := oldClass subclassDefinerClass new
				source: aString;
				requestor: aController;
				logged: true;
				evaluate.
	^ (class isKindOf: Behavior)
		ifTrue: [ class ]
		ifFalse: [ nil ]
]

{ #category : #method }
AbstractTool >> compileANewMethodInClass: aClass categorized: aCategory from: aString notifyng: aController [

	| selector existingMethod targetClass |
	
	targetClass := aClass.	
	"try to guess the selector form the first line XXX hack"
	selector := aString lines first findSelector.
	
	"Warn the user if a Trait method would be overridden"
	(aClass includesSelector: selector) ifTrue: [
		existingMethod := aClass methodNamed: selector.
		existingMethod isFromTrait ifTrue: [
			targetClass := UIManager default 
				chooseFrom:  {'Create copy in ', aClass name. 
								'Compile Trait method in ', existingMethod origin name}
				values: { aClass. existingMethod origin}
				title: 'Where do you want to compile this trait method?'.
			targetClass ifNil: [ ^ nil ]]].
	
	"Run the normal compilation either on aClass or the Trait"
	^ targetClass compile: aString classified: aCategory notifying: aController.
]

{ #category : #private }
AbstractTool >> confirmAndRemoveCategory: aCategory inClass: aClass [
	| question answer |
	question := 'Are you sure you want to\remove the protocol {1}\and all its methods?'.
	answer := self confirm: (question withCRs format: {aCategory}).
	answer ifTrue: [ aClass removeProtocol: aCategory ].
	^ answer
]

{ #category : #class }
AbstractTool >> copyClasses: aCollection [
	
	| needsRefresh lastDuplicate | 
	
	aCollection ifEmpty: [ ^ false -> nil ].
	
	needsRefresh := false.
	aCollection do: [:class || copysName |
		copysName := UIManager default request: 'Please type new class name' initialAnswer: class name.
		copysName isEmptyOrNil
			ifFalse: [
				needsRefresh ifFalse: [ needsRefresh := true ].
				lastDuplicate := class duplicateClassWithNewName: copysName ]].

	^(needsRefresh -> lastDuplicate)
]

{ #category : #method }
AbstractTool >> defaultGetSelectorAndSendQuery: array to: queryPerformer with: querySelector [
	| selector |
	
	selector := UIManager default request: 'Type selector:' initialAnswer: 'flag:'.
	selector ifNil: [ selector := String new ].
	selector := selector copyWithout: Character space.
	^ selector isEmptyOrNil
		ifFalse: [ 
			(Symbol
				hasInterned: selector
				ifTrue: [ :aSymbol | 
					array at: 1 put: aSymbol.
					queryPerformer perform: querySelector withArguments: array ])
				ifFalse: [ self inform: 'no such selector' ] ]
]

{ #category : #class }
AbstractTool >> defineTrait: defString notifying: aController [  

	| defTokens keywdIx envt oldTrait newTraitName trait |
	self selectedClass isTrait ifTrue:[oldTrait := self selectedClass].
	defTokens := defString findTokens: Character separators.
	keywdIx := defTokens findFirst: [:x | x = 'category'].
	envt := self class environment.
	keywdIx := defTokens findFirst: [:x | x = 'named:'].
	newTraitName := (defTokens at: keywdIx+1) copyWithoutAll: '#()'.
	((oldTrait isNil or: [oldTrait baseTrait name asString ~= newTraitName])
		and: [envt includesKey: newTraitName asSymbol]) ifTrue:
			["Attempting to define new class/trait over existing one when
				not looking at the original one in this browser..."
			(self confirm: ((newTraitName , ' is an existing class/trait in this system.
Redefining it might cause serious problems.
Is this really what you want to do?') asText makeBoldFrom: 1 to: newTraitName size))
				ifFalse: [^ nil ]].

	trait := self class compiler source: defString; requestor: aController; logged: true; evaluate.
	^ trait
]

{ #category : #private }
AbstractTool >> do: aBlock on: aCollection displaying: aString [

	| job |
	aCollection ifEmpty: [ ^ self ].
	job := [ :bar | 
		aCollection do: [ :element | 
			aBlock value: element.
			bar increment ] ] asJob.
	job
		title: aString;
		min: 0;
		max: aCollection size;
		run.
]

{ #category : #category }
AbstractTool >> fileOutCategories: aCollection from: aClass [

	aCollection do: [:name | aClass fileOutCategory: name ]
]

{ #category : #class }
AbstractTool >> fileOutClasses: aCollection [
	"Print a description of the selected class onto a file whose name is the 
	category name followed by .st."

	self 
		do: [ :class | class theNonMetaClass fileOut ]
		on: aCollection
		displaying: 'Filing out classes...'.
]

{ #category : #method }
AbstractTool >> fileOutMethods: aCollection [

	self 
		do: [ :method | method methodClass fileOutMethod: method selector ]
		on: aCollection
		displaying: 'Filing out methods...'.
]

{ #category : #package }
AbstractTool >> fileOutPackages: aCollection [

	self 
		do: [ :package |  package item package fileOut ]
		on: aCollection
		displaying: 'Filing out packages...'.
]

{ #category : #method }
AbstractTool >> getSelectorAndSendQuery: querySelector to: queryPerformer with: queryArgs selector: aSelector [
	"Obtain a selector relevant to the current context, and then send the querySelector to the queryPerformer with the selector obtained and queryArgs as its arguments.  If no message is currently selected, then obtain a method name from a user type-in"
	| strm array |
	
	array := Array new: queryArgs size + 1.
	strm := array writeStream.
	strm nextPut: nil.
	strm nextPutAll: queryArgs.
	
	aSelector ifNil: [ ^ self 
						defaultGetSelectorAndSendQuery: querySelector
						to: queryPerformer
						with: array ].
	
	array at: 1 put: aSelector.
	queryPerformer perform: querySelector withArguments: array.
]

{ #category : #icon }
AbstractTool >> icon: aSymbol [
	"Return the icon associated with the argument."
	self flag: #remove.
	^ self iconNamed: aSymbol
]

{ #category : #package }
AbstractTool >> isValidPackageName: name [

	^ (name isNil or: [ name isEmpty or: [ name first = $* ]]) not
]

{ #category : #method }
AbstractTool >> methodHierarchyFrom: aMethod [
	"Create and schedule a method browser on the hierarchy of implementors."

	aMethod ifNil: [ ^ self ].
	self systemNavigation 
			methodHierarchyBrowserForClass: aMethod methodClass
			selector: aMethod selector
]

{ #category : #method }
AbstractTool >> moveMethod: aMethod inPackage: aPackage [

	aMethod methodClass organization
				classify: aMethod selector
				under: '*', aPackage name
				suppressIfDefault: true.
]

{ #category : #category }
AbstractTool >> removeCategory: aCategory inClass: aClass [
	| protocol |
	aCategory ifNil: [ ^ false ].
	self okToChange ifFalse: [ ^ false].
	protocol := aClass organization protocolNamed: aCategory.
	protocol isVirtualProtocol ifTrue:[^ false ].
	protocol isEmpty ifTrue: [ aClass removeProtocol: aCategory. ^ true ].
	
	^ self confirmAndRemoveCategory: aCategory inClass: aClass
	
]

{ #category : #class }
AbstractTool >> removeClass: aClass [
	"Remove the selected class from the system, at interactive user request.  Make certain the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| message className classToRemove result |
	aClass ifNil: [ ^ false ].
	classToRemove := aClass theNonMetaClass.
	className := classToRemove name.
	message := self removeClassMessageFor: className.
	(result := self confirm: message)
		ifTrue: [ 
			classToRemove subclasses notEmpty
				ifTrue: [ 
					(self confirm: 'class has subclasses: ' , message)
						ifFalse: [ ^ false ] ].
			classToRemove removeFromSystem ].
	^ result
]

{ #category : #class }
AbstractTool >> removeClassMessageFor: className [
	^ 'Are you certain that you
want to REMOVE the class ' , className
		,
			'
from the system?'
]

{ #category : #class }
AbstractTool >> removeClasses: aCollection [
	"Remove the selected classes from the system.  Check that the user really wants to do this, since it is not reversible.  Answer true if removal actually happened."

	| classNames classesToRemove result |
	aCollection isEmptyOrNil
		ifTrue: [ ^ false ].
	classesToRemove := aCollection collect: #theNonMetaClass.
	classNames := (classesToRemove collect: #name) joinUsing: ', '.
	(result := self confirm: (self removeClassesMessageFor: classNames))
		ifTrue: [ 
			classesToRemove
				do: [ :classToRemove | 
					classToRemove subclasses notEmpty
						ifTrue: [ 
							(self confirm: (self removedClassHasSubclassesMessageFor: classToRemove name))
								ifTrue: [ classToRemove removeFromSystem ] ]
						ifFalse: [ classToRemove removeFromSystem ] ] ].
	^ result
]

{ #category : #class }
AbstractTool >> removeClassesMessageFor: classNames [
	^ 'Are you certain that you
want to REMOVE the classes ' , classNames
		,
			'
from the system?'
]

{ #category : #category }
AbstractTool >> removeEmptyCategoriesFrom: aClass [

	aClass organization removeEmptyCategories.
]

{ #category : #category }
AbstractTool >> removeEmptyUnclassifiedCategoryFrom: aClass [
	"Remove the 'as yet unclassified' category from the class if empty"

	| protocolOrganizer |
	protocolOrganizer := aClass organization protocolOrganizer.
	protocolOrganizer protocols
		detect: [ :protocol | protocol name = Protocol unclassified and: [ protocol isEmpty ] ]
		ifFound: [ :unclassifiedProtocol | protocolOrganizer removeProtocol: unclassifiedProtocol ]
]

{ #category : #method }
AbstractTool >> removeMethod: aMethod inClass: aClass [
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so, remove it. "
	| messageName confirmation |
	
	aMethod ifNil: [^ false].
	messageName := aMethod selector.
	confirmation := self systemNavigation confirmRemovalOf: messageName on: aClass.
	confirmation = 3
		ifTrue: [^ false].
	(aClass includesLocalSelector: messageName)
		ifTrue: [ aClass removeSelector: messageName ]
		ifFalse: [ self removeNonLocalSelector: messageName ].

	"In case organization not cached"
	confirmation = 2
		ifTrue: [self systemNavigation browseAllSendersOf: messageName].
		
	self removeEmptyUnclassifiedCategoryFrom: aClass.
	^ true
]

{ #category : #method }
AbstractTool >> removeMethods: aCollection inClass: aClass [
	"If a message is selected, create a Confirmer so the user can verify that  
	the currently selected message should be removed from the system. If 
	so, remove it. "
	| messagesName confirmation |
	
	aCollection isEmptyOrNil ifTrue: [^ false].
	aCollection size = 1
		ifTrue: [ ^ self removeMethod: aCollection first inClass: aClass ].
	messagesName := aCollection collect: #selector.
	confirmation := self systemNavigation confirmRemovalOfSelectors: messagesName on: aClass.
	confirmation = 3
		ifTrue: [ ^ false ].
	aCollection do: [:message|| messageName |	
		messageName := message selector.
		(aClass includesLocalSelector: messageName)
			ifTrue: [ aClass removeSelector: messageName ]
			ifFalse: [ self removeNonLocalSelector: messageName ]].

	"In case organization not cached"
	confirmation = 2
		ifTrue: [ 
			aCollection do: [:message || messageName |	
				messageName := message selector.
				((self systemNavigation allCallsOn: messageName) notEmpty)
					ifTrue: [ self systemNavigation browseAllSendersOf: messageName ]]].
	^ true
]

{ #category : #class }
AbstractTool >> removedClassHasSubclassesMessageFor: className [

	^ className, ' has subclasses.
Do you really want to REMOVE it from the system ?'
]

{ #category : #category }
AbstractTool >> renameCategory: aCategory from: aClass [
	"Prompt for a new category name and add it before the
	current selection, or at the end if no current selection"
	
	| oldName newName |
	
	aClass ifNil: [^ nil].
	aCategory ifNil: [^ nil].
	
	oldName := aCategory.
	newName := self class requestProtocolNameFor: aClass initialAnswer: oldName.
	newName isEmptyOrNil
		ifTrue: [^ nil ]
		ifFalse: [newName := newName asSymbol].
	newName = oldName ifTrue: [^ nil ].
	aClass organization
		renameCategory: oldName
		toBe: newName.
	
	^ newName
]

{ #category : #class }
AbstractTool >> renameClass: aClass [
	| oldName newName obs |

	aClass ifNil: [ ^ self ].
	
	oldName := aClass name.
	newName := UIManager default  request: 'Please type new class name' initialAnswer: oldName.
	newName isEmptyOrNil ifTrue: [ ^ self ].	"Cancel returns"
	newName := newName asSymbol.
	newName = oldName ifTrue: [ ^ self ].
	(self class environment includesKey: newName)
		ifTrue: [ ^ self error: newName , ' already exists' ].
	aClass rename: newName.

	obs := self systemNavigation allReferencesTo: (aClass environment associationAt: newName).
	obs isEmpty
		ifFalse: [ self systemNavigation browseMessageList: obs name: 'Obsolete References to ' , oldName autoSelect: oldName ].
	^ newName
]

{ #category : #private }
AbstractTool >> renamePackage: anRPackage [

	| newName |
	newName := UIManager default 
		request: 'New name of the package' 
		initialAnswer: anRPackage name 
		title: 'Rename a package'.
	newName = anRPackage name ifTrue: [ ^ self ].
	(self isValidPackageName: newName)
		ifTrue: [ anRPackage renameTo: newName ]
		ifFalse: [ self alertInvalidPackageName:newName ].
]

{ #category : #class }
AbstractTool >> showUnreferencedClassVarsOf: aClass [
	"Search for all class variables known to the selected class, and put up a 
	list of those that have no references anywhere in the system. The 
	search includes superclasses, so that you don't need to navigate your 
	way to the class that defines each class variable in order to determine 
	whether it is unreferenced"

	| aList aReport |
	(aClass isNil or: [ aClass isTrait ])
		ifTrue: [ ^ self ].
	aList := self systemNavigation allUnreferencedClassVariablesOf: aClass.
	aList isEmpty
		ifTrue: [ 
			^ UIManager default
				inform:
					'There are no unreferenced
class variables in
' , aClass name ].
	aReport := String
		streamContents: [ :aStream | 
			aStream
				nextPutAll: 'Unreferenced class variable(s) in ' translated;
				nextPutAll: aClass name;
				cr.
			aList
				do: [ :el | 
					aStream
						tab;
						nextPutAll: el;
						cr ] ].
	UIManager default inform: aReport
]

{ #category : #class }
AbstractTool >> showUnreferencedInstVarsOf: aClass [
	"Search for all instance variables known to the selected class, and put up a list of those that have no references anywhere in the system.  The search includes superclasses, so that you don't need to navigate your way to the class that defines each inst variable in order to determine whether it is unreferenced"

	|  aList aReport |
	(aClass isNil or: [aClass isTrait]) ifTrue: [^ self].
	aList := aClass allUnreferencedInstanceVariables.
	aList isEmpty ifTrue: [^ UIManager default inform: 'There are no unreferenced
instance variables in
', aClass name].
	aReport := String streamContents:
		[:aStream |
			aStream nextPutAll: 'Unreferenced instance variable(s) in ' translated, aClass name; cr.
			aList do: [:el | aStream tab; nextPutAll: el; cr]].
	UIManager default inform: aReport
]
