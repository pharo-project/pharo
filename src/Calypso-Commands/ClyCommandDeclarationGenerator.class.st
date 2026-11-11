"
I convert the class annotations of Calypso commands (ClyCommandActivationStrategy) into declarations with the <calypsoCommands> pragma (see ClyCommandMap), for the commands of some packages.
I write one class-side method per anchor and per package of commands, as an extension of the anchor when it belongs to another package. Each annotation becomes an entry with the same group, order, label, key, mouse button, drop anchor and priority. An annotation inherited by all the concrete subclasses of an abstract command, with nothing specific to one of them, becomes a single #addAllConcreteSubclassesOf:.
Only the annotations of strategies that have an interaction (#interactionName) are converted. The ones of the strategies of the commands opening sub-menus are not.
Usage:
	generator := ClyCommandDeclarationGenerator forPackagesNamed: #('SystemCommands-MethodCommands').
	generator generateDeclarations.
	generator mismatches. ""must be empty""
	generator removeAnnotationMethods.
#mismatches compares every converted annotation with the entry declared for it, so the conversion can be checked before the annotations are removed. #undo removes the generated declarations and compiles back the removed annotation methods.
Annotation methods which also annotate commands of other projects through inheritance are not converted (see #commandPackagePrefixes).
"
Class {
	#name : 'ClyCommandDeclarationGenerator',
	#superclass : 'Object',
	#instVars : [
		'packageNames',
		'annotations',
		'removedMethods',
		'generatedMethods',
		'commandPackagePrefixes'
	],
	#category : 'Calypso-Commands',
	#package : 'Calypso-Commands'
}

{ #category : 'instance creation' }
ClyCommandDeclarationGenerator class >> forPackagesNamed: packageNames [

	^ self new setPackageNames: packageNames
]

{ #category : 'accessing' }
ClyCommandDeclarationGenerator class >> interactionOrder [
	"The order of the interactions in a declaration method."

	^ #( #contextMenu #toolbar #tabActions #tableIcons #shortcuts #click #doubleClick #dragAndDrop #textLeftBarMenu #textLeftBarClick #textLeftBarDoubleClick )
]

{ #category : 'accessing' }
ClyCommandDeclarationGenerator class >> menuInteractions [

	^ #( #contextMenu #toolbar #tabActions #textLeftBarMenu )
]

{ #category : 'accessing' }
ClyCommandDeclarationGenerator class >> mouseInteractions [

	^ #( #click #doubleClick #textLeftBarClick #textLeftBarDoubleClick )
]

{ #category : 'accessing' }
ClyCommandDeclarationGenerator >> annotations [
	"The class annotations I convert."

	^ annotations ifNil: [ annotations := self collectAnnotations ]
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> canDeclare: conversions asFamily: aCommandClass [

	| commandClasses |
	aCommandClass isAbstract ifFalse: [ ^ false ].
	conversions size > 1 ifFalse: [ ^ false ].
	(conversions allSatisfy: [ :conversion | conversion hasExtras not and: [ conversion expression = ('add: ' , conversion annotations first annotatedClass name) ] ]) ifFalse: [ ^ false ].
	(conversions collect: [ :conversion | conversion anchor -> conversion group ]) asSet size = 1 ifFalse: [ ^ false ].
	commandClasses := conversions collect: [ :conversion | conversion annotations first annotatedClass ].
	^ commandClasses asSet = (aCommandClass allSubclasses reject: [ :commandClass | commandClass isAbstract ]) asSet
]

{ #category : 'source' }
ClyCommandDeclarationGenerator >> characterSourceOf: aSingleKeyCombination [
	"A letter is written as a character literal. The other keys of the annotations are the arrows."

	| keyName |
	keyName := aSingleKeyCombination key name.
	keyName size = 1 ifTrue: [ ^ (Character value: keyName first asLowercase asInteger) printString ].
	^ 'Character arrow' , keyName asLowercase capitalized
]

{ #category : 'private' }
ClyCommandDeclarationGenerator >> collectAnnotations [
	"The annotations defined by methods of my packages. A declaration goes in the package of the method that defined the annotation, which may be an extension of the command. A method that also annotates commands of other projects, through inheritance, is left as it is: its declarations belong to these projects."

	| candidates |
	candidates := (ClyCommandActivationStrategy allSubclasses select: [ :strategy | strategy interactionName isNotNil ]) flatCollect: [ :strategy |
		              (strategy registry allInstancesOf: strategy) select: [ :annotation | packageNames includes: (self declaringMethodOf: annotation) package name ] ].
	^ ((candidates groupedBy: [ :annotation | self declaringMethodOf: annotation ]) values select: [ :someAnnotations |
		   someAnnotations allSatisfy: [ :annotation | self isCommandOfConvertedProjects: annotation annotatedClass ] ]) flatCollect: [ :someAnnotations | someAnnotations ]
]

{ #category : 'accessing' }
ClyCommandDeclarationGenerator >> commandPackagePrefixes [
	"The commands of the projects I convert. Calypso and SystemCommands by default."

	^ commandPackagePrefixes ifNil: [ #( 'Calypso-' 'SystemCommands-' ) ]
]

{ #category : 'accessing' }
ClyCommandDeclarationGenerator >> commandPackagePrefixes: someStrings [

	commandPackagePrefixes := someStrings
]

{ #category : 'generating' }
ClyCommandDeclarationGenerator >> compileDeclaration: aSource on: anAnchorClass inPackageNamed: aPackageName [

	| protocol selector method |
	protocol := anAnchorClass package name = aPackageName
		            ifTrue: [ 'commands' ]
		            ifFalse: [ '*' , aPackageName ].
	selector := anAnchorClass classSide compile: aSource classified: protocol.
	method := anAnchorClass classSide >> selector.
	anAnchorClass classSide compile: (method ast formattedCode copyReplaceAll: String lf with: String cr) classified: protocol.
	method := anAnchorClass classSide >> selector.
	generatedMethods ifNil: [ generatedMethods := OrderedCollection new ].
	generatedMethods add: method.
	^ method
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> conversionOf: anAnnotation [

	| interaction commandClass expression extras group |
	interaction := anAnnotation class interactionName.
	commandClass := anAnnotation annotatedClass.
	group := ClyRootMenuGroup.
	expression := 'add: ' , commandClass name.
	extras := OrderedCollection new.
	(self class menuInteractions includes: interaction) ifTrue: [
			group := anAnnotation menuGroup.
			(self needsOrder: anAnnotation) ifTrue: [ expression := expression , ' order: ' , anAnnotation menuItemOrder printString ].
			anAnnotation menuItemName ifNotNil: [ :label | extras add: 'label: ' , label printString ] ].
	interaction = #shortcuts ifTrue: [ expression := expression , ' key: ' , (self keySourceOf: anAnnotation keyCombination) ].
	interaction = #dragAndDrop ifTrue: [ expression := expression , ' dropIn: ' , anAnnotation activeDropContext commandAnchor name ].
	(self class mouseInteractions includes: interaction) ifTrue: [
			anAnnotation keyboardModifier ifNotNil: [ self error: 'Keyboard modifiers of mouse annotations are not converted' ].
			anAnnotation isYellowButton ifTrue: [ extras add: 'beYellowButton' ].
			anAnnotation isBlueButton ifTrue: [ extras add: 'beBlueButton' ] ].
	anAnnotation priority = 0 ifFalse: [ extras add: 'priority: ' , anAnnotation priority printString ].
	^ ClyAnnotationConversion new
		  anchor: anAnnotation activeContext commandAnchor;
		  packageName: (self declaringMethodOf: anAnnotation) package name;
		  interaction: interaction;
		  group: group;
		  expression: expression;
		  extras: extras;
		  annotations: { anAnnotation };
		  yourself
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> conversions [
	"One conversion per annotation, except for the families declared with #addAllConcreteSubclassesOf:."

	^ (self annotations groupedBy: [ :annotation | annotation class -> (self declaringMethodOf: annotation) ]) values flatCollect: [ :someAnnotations | self conversionsOfDeclaration: someAnnotations ]
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> conversionsOfDeclaration: someAnnotations [
	"The annotations come from the same method. When it is inherited by every concrete subclass of an abstract command, and nothing is specific to one of them, they become one #addAllConcreteSubclassesOf:."

	| conversions family |
	conversions := someAnnotations collect: [ :annotation | self conversionOf: annotation ].
	family := (self declaringMethodOf: someAnnotations anyOne) methodClass instanceSide.
	(self canDeclare: conversions asFamily: family) ifFalse: [ ^ conversions ].
	^ { (ClyAnnotationConversion new
		   anchor: conversions first anchor;
		   packageName: conversions first packageName;
		   interaction: conversions first interaction;
		   group: conversions first group;
		   expression: 'addAllConcreteSubclassesOf: ' , family name;
		   extras: #(  );
		   annotations: someAnnotations;
		   yourself) }
]

{ #category : 'generating' }
ClyCommandDeclarationGenerator >> declarationSources [
	"The source of each declaration method to generate, by anchor and package name."

	| sources |
	sources := Dictionary new.
	(self conversions groupedBy: [ :conversion | conversion anchor -> conversion packageName ]) keysAndValuesDo: [ :anchorAndPackage :conversions |
		sources at: anchorAndPackage put: (self sourceOfDeclaration: conversions selector: (self selectorForPackageNamed: anchorAndPackage value)) ].
	^ sources
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> declaringMethodOf: anAnnotation [

	^ anAnnotation annotatedClass classSide lookupSelector: anAnnotation declarationSelector
]

{ #category : 'checking' }
ClyCommandDeclarationGenerator >> entry: anEntry matches: anAnnotation [

	| interaction |
	interaction := anAnnotation class interactionName.
	anEntry priority = anAnnotation priority ifFalse: [ ^ false ].
	(self class menuInteractions includes: interaction) ifTrue: [
			anEntry group = anAnnotation menuGroup ifFalse: [ ^ false ].
			anEntry label = anAnnotation menuItemName ifFalse: [ ^ false ].
			^ anEntry order = anAnnotation menuItemOrder or: [ anEntry order isNil and: [ (self needsOrder: anAnnotation) not ] ] ].
	interaction = #shortcuts ifTrue: [ ^ anEntry keyCombination = anAnnotation keyCombination ].
	interaction = #dragAndDrop ifTrue: [ ^ anEntry dropAnchor = anAnnotation activeDropContext commandAnchor ].
	(self class mouseInteractions includes: interaction) ifTrue: [ ^ anEntry printWhichButton = anAnnotation printWhichButton and: [ anEntry keyboardModifier = anAnnotation keyboardModifier ] ].
	^ true
]

{ #category : 'generating' }
ClyCommandDeclarationGenerator >> generateDeclarations [
	"Compiles the declaration methods on the class side of the anchors, formatted. Answers them."

	^ self declarationSources associations collect: [ :association | self compileDeclaration: association value on: association key key inPackageNamed: association key value ]
]

{ #category : 'private' }
ClyCommandDeclarationGenerator >> isCommandOfConvertedProjects: aCommandClass [

	^ self commandPackagePrefixes anySatisfy: [ :prefix | aCommandClass package name beginsWith: prefix ]
]

{ #category : 'source' }
ClyCommandDeclarationGenerator >> keySourceOf: aKeyCombination [
	"The source of an expression which answers a key combination equal to aKeyCombination, such as $e meta , $m meta."

	aKeyCombination class = KMKeyCombinationSequence ifTrue: [ ^ ' , ' join: (aKeyCombination shortcuts collect: [ :each | self keySourceOf: each ]) ].
	aKeyCombination class = KMModifiedKeyCombination ifTrue: [
		^ (self characterSourceOf: aKeyCombination character) , ' ' , (' ' join: ((aKeyCombination modifier printString substrings: ' +') collect: [ :modifierName | modifierName asLowercase ])) ].
	aKeyCombination class = KMSingleKeyCombination ifTrue: [ ^ (self characterSourceOf: aKeyCombination) , ' asShortcut' ].
	^ self error: 'Unsupported key combination: ' , aKeyCombination printString
]

{ #category : 'checking' }
ClyCommandDeclarationGenerator >> mismatches [
	"The converted annotations which have no entry with the same placement, declared on their anchor for their interaction. Run it after #generateDeclarations: it must be empty before the annotations are removed."

	| registry |
	ClyCommandRegistry reset.
	registry := ClyCommandRegistry default.
	^ self annotations reject: [ :annotation |
			  (((registry mapOf: annotation activeContext commandAnchor) entriesOf: annotation class interactionName) select: [ :entry | entry commandClass == annotation annotatedClass ]) anySatisfy: [ :entry |
				  self entry: entry matches: annotation ] ]
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> needsOrder: aMenuAnnotation [
	"An entry without order asks its command, which answers 1000 unless the command defines its own order."

	^ aMenuAnnotation menuItemOrder ~= 1000 or: [ (self usesDefaultOrder: aMenuAnnotation annotatedClass) not ]
]

{ #category : 'removing' }
ClyCommandDeclarationGenerator >> overriddenAnnotationMethodsOf: aMethod [

	^ aMethod methodClass allSuperclasses
		  select: [ :superclass | (superclass includesSelector: aMethod selector) and: [ superclass >> aMethod selector hasPragmaNamed: #classAnnotation ] ]
		  thenCollect: [ :superclass | superclass >> aMethod selector ]
]

{ #category : 'removing' }
ClyCommandDeclarationGenerator >> removeAnnotationMethod: aMethod [
	"Does nothing when the method was already removed, as an overridden method of another one."

	| selector |
	selector := aMethod selector.
	(aMethod methodClass includesSelector: selector) ifFalse: [ ^ self ].
	(aMethod methodClass allSubclasses select: [ :subclass | (subclass includesSelector: selector) and: [ (subclass >> selector) ast statements isEmpty ] ]) do: [ :subclass |
		self removeMethod: subclass >> selector ].
	self removeMethod: aMethod methodClass >> selector
]

{ #category : 'removing' }
ClyCommandDeclarationGenerator >> removeAnnotationMethods [
	"Removes the methods which defined the converted annotations, and the empty overrides that disabled them in subclasses. A method is kept when it also defines annotations which are not converted, or when it overrides an annotation method of a superclass that still defines annotations which are not converted, since removing it would annotate its subclasses again. The overridden methods which define only converted annotations, or none, are removed too. Answers the kept methods."

	| allAnnotations kept definesOnlyConverted |
	allAnnotations := ClyCommandActivationStrategy allSubclasses flatCollect: [ :strategy | strategy registry allInstancesOf: strategy ].
	definesOnlyConverted := [ :method | (allAnnotations select: [ :annotation | (self declaringMethodOf: annotation) = method ]) allSatisfy: [ :annotation | self annotations includes: annotation ] ].
	kept := OrderedCollection new.
	((self annotations collect: [ :annotation | self declaringMethodOf: annotation ]) asSet reject: [ :method | method isNil ]) do: [ :method |
			| overridden |
			overridden := self overriddenAnnotationMethodsOf: method.
			((definesOnlyConverted value: method) and: [ overridden allSatisfy: definesOnlyConverted ])
				ifTrue: [ { method } , overridden do: [ :each | self removeAnnotationMethod: each ] ]
				ifFalse: [ kept add: method ] ].
	^ kept
]

{ #category : 'removing' }
ClyCommandDeclarationGenerator >> removeMethod: aMethod [

	removedMethods ifNil: [ removedMethods := OrderedCollection new ].
	removedMethods add: {
			aMethod methodClass.
			aMethod sourceCode.
			aMethod protocolName }.
	aMethod methodClass removeSelector: aMethod selector
]

{ #category : 'removing' }
ClyCommandDeclarationGenerator >> restoreAnnotationMethods [
	"Compiles back the methods I removed, to undo a conversion with the declarations I generated."

	removedMethods ifNil: [ ^ self ].
	removedMethods reverseDo: [ :removed | (removed at: 1) compile: (removed at: 2) classified: (removed at: 3) ].
	removedMethods := nil
]

{ #category : 'source' }
ClyCommandDeclarationGenerator >> selectorForPackageNamed: aPackageName [
	"SystemCommands-MethodCommands gives #systemCommandsMethodCommandsIn: and Calypso-SystemTools-FullBrowser gives #calypsoSystemToolsFullBrowserCommandsIn:."

	| camelCase |
	camelCase := String streamContents: [ :stream |
			             (aPackageName substrings: '-') withIndexDo: [ :part :index |
					             stream nextPutAll: (index = 1
							              ifTrue: [ part uncapitalized ]
							              ifFalse: [ part ]) ] ].
	(camelCase endsWith: 'Commands') ifFalse: [ camelCase := camelCase , 'Commands' ].
	^ (camelCase , 'In:') asSymbol
]

{ #category : 'initialization' }
ClyCommandDeclarationGenerator >> setPackageNames: aCollection [

	packageNames := aCollection asSet
]

{ #category : 'source' }
ClyCommandDeclarationGenerator >> sourceOfDeclaration: conversions selector: aSelector [
	"The statements go by interaction, then by menu group (the root group first), then by expression. Commands without extra messages are added in one cascade per group."

	| statements |
	statements := OrderedCollection new.
	self class interactionOrder do: [ :interaction |
			| ofInteraction groups |
			ofInteraction := conversions select: [ :conversion | conversion interaction = interaction ].
			groups := (ofInteraction collect: [ :conversion | conversion group ]) asSet asArray sorted: [ :first :second |
				          first = ClyRootMenuGroup or: [ second ~= ClyRootMenuGroup and: [ first name <= second name ] ] ].
			groups do: [ :group |
					| ofGroup plain |
					ofGroup := (ofInteraction select: [ :conversion | conversion group = group ]) sorted: [ :first :second | first expression <= second expression ].
					plain := ofGroup reject: [ :conversion | conversion hasExtras ].
					plain ifNotEmpty: [ statements add: plain first receiverSource , ' ' , ('; ' join: (plain collect: [ :conversion | conversion expression ])) ].
					(ofGroup select: [ :conversion | conversion hasExtras ]) do: [ :conversion | statements add: conversion statementSource ] ] ].
	^ String streamContents: [ :stream |
			  stream
				  nextPutAll: aSelector;
				  nextPutAll: ' aCommandMap';
				  cr;
				  cr;
				  tab;
				  nextPutAll: '<calypsoCommands>'.
			  statements
				  do: [ :statement |
						  stream
							  cr;
							  tab;
							  nextPutAll: statement ]
				  separatedBy: [ stream nextPut: $. ] ]
]

{ #category : 'removing' }
ClyCommandDeclarationGenerator >> undo [
	"Removes the declarations I generated and compiles back the annotation methods I removed."

	generatedMethods ifNotNil: [
			generatedMethods do: [ :method | method methodClass removeSelector: method selector ].
			generatedMethods := nil ].
	self restoreAnnotationMethods
]

{ #category : 'converting' }
ClyCommandDeclarationGenerator >> usesDefaultOrder: aCommandClass [

	^ (aCommandClass lookupSelector: #orderInContext:) methodClass = ClyCommand and: [ (aCommandClass lookupSelector: #order) methodClass = ClyCommand ]
]
