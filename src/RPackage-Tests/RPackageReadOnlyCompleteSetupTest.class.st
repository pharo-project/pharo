"
The tests takes as fixture the following situation and exercises the readonly queries.
We should be able to use a test resources to speed it up.

P1 
	A1DefinedInP1
	A1DefinedInP1>>methodDefinedInP1
	B1DefinedInP1	
	A2DefinedInP2>>methodDefinedInP1
	
P2
	A2DefinedInP2
	A2DefinedInP2>>methodDefinedInP2
	B2DefinedInB2	

P3
	A3DefinedInP3	
	A2DefinedInP2>>methodDefinedInP3 
"
Class {
	#name : #RPackageReadOnlyCompleteSetupTest,
	#superclass : #RPackageIncrementalTest,
	#instVars : [
		'p1',
		'p2',
		'p3',
		'a1',
		'b1',
		'a2',
		'a3',
		'b2',
		'p4'
	],
	#category : #'RPackage-Tests'
}

{ #category : #expectedfailures }
RPackageReadOnlyCompleteSetupTest >> expectedFailures [

	^ #(testClassNamesAndExtensionSelectorsDo testClassNamesAndExtensionSelectorsDo)
]

{ #category : #running }
RPackageReadOnlyCompleteSetupTest >> setUp [

	super setUp.
	p1 := self createNewPackageNamed: self p1Name.
	p2 := self createNewPackageNamed: self p2Name.
	p3 := self createNewPackageNamed: self p3Name.
	
	a1 := self createNewClassNamed: #A1DefinedInP1 inPackage: p1.
	b1 := self createNewClassNamed: #B1DefinedInP1 inPackage: p1.
	a2 := self createNewClassNamed: #A2DefinedInP2 inPackage: p2.
	b2 := self createNewClassNamed: #B2DefinedInP2 inPackage: p2.
	a3 := self createNewClassNamed: #A3DefinedInP3 inPackage: p3.
	
	a1 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a1>>#methodDefinedInP1).
	a1 compileSilently: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.
	p1 addMethod: (a1>>#anotherMethodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	p1 addMethod: (a2>>#methodDefinedInP1).
	
	a2 compileSilently: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	p2 addMethod: (a2>>#methodDefinedInP2).
	
	a2 compileSilently: 'methodDefinedInP3 ^ #methodDefinedInP3'.
	p3 addMethod: (a2>>#methodDefinedInP3).

	a2 class compileSilently: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3'.
	p3 addMethod: (a2 class>>#classSideMethodDefinedInP3).
	

]

{ #category : #'test tag class' }
RPackageReadOnlyCompleteSetupTest >> testAddTag [
	self assert: p1 classTags isEmpty.
	p1 addClassTag: #baz. 
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (p1 classTags size = 1).
	
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: (p1 classTags size = 2).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 2).
	
	p1 addClassTag: #foo. 
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 2).
]

{ #category : #'test tag class' }
RPackageReadOnlyCompleteSetupTest >> testAddTagNames [
	
	self assert: p1 classTags isEmpty.
	p1 addClassTag: #baz. 
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (p1 classTags size = 1).
	
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: (p1 classTags size = 2).
	self assert: ((p1 classNamesForClassTag: #foo) includes: #A1DefinedInP1).
	self assert: ((p1 classNamesForClassTag: #foo) includes: #B1DefinedInP1).
	self assert: ((p1 classNamesForClassTag: #foo) size = 2).
	
	p1 addClassTag: #foo. 
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: ((p1 classNamesForClassTag: #foo) includes: #A1DefinedInP1).
	self assert: ((p1 classNamesForClassTag: #foo) includes: #B1DefinedInP1).
	self assert: ((p1 classNamesForClassTag: #foo) size = 2).
]

{ #category : #'test tag class' }
RPackageReadOnlyCompleteSetupTest >> testAddTagsToAClass [
	self assert: p1 classTags isEmpty.
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 1).
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 2).

	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #zork.
	self assert: (((p1 classesForClassTag: #zork) collect: #name) includes: #B1DefinedInP1).
	self assert: ((p1 classesForClassTag: #foo) size = 2).
	self assert: ((p1 classesForClassTag: #zork) size = 1).
]

{ #category : #'test compiled method' }
RPackageReadOnlyCompleteSetupTest >> testClassIsExtendedInPackage [
	self deny: ((Smalltalk globals at: #A1DefinedInP1) isExtendedInPackage: p1).
	self assert: (p1 includesClass: (Smalltalk globals at: #A1DefinedInP1)).
	self deny: (p1 extendsClass: (Smalltalk globals at: #A1DefinedInP1)).
	self assert: ((Smalltalk globals at: #A2DefinedInP2) isExtendedInPackage: p1).
	self deny: (p1 includesClass: (Smalltalk globals at: #A2DefinedInP2)).
	self assert: (p1 extendsClass: (Smalltalk globals at: #A2DefinedInP2)).
	
]

{ #category : #'test slice' }
RPackageReadOnlyCompleteSetupTest >> testClassNamesAndExtensionSelectorsDo2 [
	self assert: (String streamContents: [:stream |
		{p1 . p2 .p3} do: [:p |
			p classNamesAndExtensionSelectorsDo: [:aClassName :selector |
				stream nextPutAll: aClassName  ; nextPutAll: ' ' ; nextPutAll:  selector asString; cr ]]])
		= 'A2DefinedInP2 methodDefinedInP1
A2DefinedInP2 methodDefinedInP3
A2DefinedInP2_class classSideMethodDefinedInP3
'.

]

{ #category : #'test slice' }
RPackageReadOnlyCompleteSetupTest >> testClassesAndExtensionMethodsDo [
	| block |
	self assert: (String streamContents: [:stream |
		block := [ :aClassName :selectors |
				stream nextPutAll: aClassName ; nextPutAll: ' '.
				selectors do: [ :selector | stream nextPutAll: selector asString].
				stream cr.].
		{p1 . p2 . p3} do: [ :p |
			p classExtensionSlicesDo: block.
			p metaclassExtensionSlicesDo: block.]]) 
		= 'A2DefinedInP2 methodDefinedInP1
A2DefinedInP2 methodDefinedInP3
A2DefinedInP2 classSideMethodDefinedInP3
'
]

{ #category : #'test compiled method' }
RPackageReadOnlyCompleteSetupTest >> testCompiledMethodIsDefinedInPackage [

	self assert: (((Smalltalk at: #A1DefinedInP1) >> #methodDefinedInP1) isDefinedInPackage: p1).
	self deny: (((Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1) isDefinedInPackage: p1).
	
]

{ #category : #'test compiled method' }
RPackageReadOnlyCompleteSetupTest >> testCompiledMethodIsExtensionInPackage [

	self deny: (((Smalltalk at: #A1DefinedInP1) >> #methodDefinedInP1) isExtensionInPackage: p1).
	self assert: (((Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1) isExtensionInPackage: p1).
	
]

{ #category : #'test compiled method' }
RPackageReadOnlyCompleteSetupTest >> testCompiledMethodPackageFromOrganizer [
	
	| method |
	method := (Smalltalk at: #A1DefinedInP1) >> #methodDefinedInP1.
	self assert: (method packageFromOrganizer: self packageClass organizer) = p1.
	method := (Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1.
	self assert: (method packageFromOrganizer: self packageClass organizer) = p1
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testDefinedSelectors [

	self assert: a1 definedSelectors size = 2. 
	self assert: ((a1 definedSelectors) includes: #methodDefinedInP1).
	self assert: ((a1 definedSelectors) includes: #anotherMethodDefinedInP1). 

	self assert: a2 definedSelectors size = 1.
	self assert: (a2 definedSelectors includes: #methodDefinedInP2).
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testDefinedSelectorsForClass [

	self assert: (p1 definedSelectorsForClass: a1) size = 2. 
	self assert: (p1 definedMethodsForClass: a1) size = 2. 
	self assert: ((p1 definedSelectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((p1 definedSelectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assert: ((p1 definedMethodsForClass: a1) includes: ((Smalltalk at: #A1DefinedInP1)>>#methodDefinedInP1)).
	self assert: (p1 definedSelectorsForClass: Object) isEmpty. 
	self assert: (p1 definedSelectorsForClass: Object class) isEmpty. 

]

{ #category : #'test tag class' }
RPackageReadOnlyCompleteSetupTest >> testEmpty [
		
	self assert: p1 classTags isEmpty.
	
]

{ #category : #'test accessing' }
RPackageReadOnlyCompleteSetupTest >> testExtendingPackagesOfClass [
	"since a class can be extended by several packages, we want the list of packages that extend
	the class"
	
	| packages |
	packages := a2 extendingPackages.
	"a2 is extended by p1 and p3"
	self assert: packages size = 2. 
	self assert: (packages includes: p1).
	self deny: (packages includes: p2).
	self assert: (packages includes: p3).
	
	packages := a1 extendingPackages.
	self assert: packages isEmpty. 
]

{ #category : #'test accessing' }
RPackageReadOnlyCompleteSetupTest >> testExtensionMethods [
	"a package can extend several classes, either the class or  the meta-class side. 'extensionMethods' should list all the methods involved in shuch extensions. P3 extend a2 and a2 class"

	| package extensionMethods cpt |
	package := p3.
	extensionMethods := package extensionMethods.	"self assert: (extensionMethods size = 2)."
	self assert: (extensionMethods includes: a2 >> #methodDefinedInP3).
	self assert: (extensionMethods includes: a2 class >> #classSideMethodDefinedInP3).
	cpt := 0.
	package
		metaclassExtensionSlicesDo: [ :className :listOfSelectors | 
			(className = a2 name and: [ listOfSelectors includes: #classSideMethodDefinedInP3 ])
				ifTrue: [ cpt := cpt + 1 ] ].
	self assert: cpt = 1.
	cpt := 0.
	package
		classExtensionSlicesDo: [ :className :listOfSelectors | 
			(className = a2 name and: [ listOfSelectors includes: #methodDefinedInP3 ])
				ifTrue: [ cpt := cpt + 1 ] ].
	self assert: cpt = 1
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testExtensionSelectors [
	
	self assert: a1 extensionSelectors isEmpty.

	self assert: a2 extensionSelectors size = 2.
	self assert: (a2 extensionSelectors includes: #methodDefinedInP1).
	self assert: (a2 extensionSelectors includes: #methodDefinedInP3).
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testExtensionSelectorsForClass [
	
	self assert: (p1 extensionSelectorsForClass: a2) size = 1. 
	self assert: ((p1 extensionSelectorsForClass: a2) includes: #methodDefinedInP1).
	self assert: ((p1 extensionMethodsForClass: a2) includes: (Smalltalk at: #A2DefinedInP2) >> #methodDefinedInP1).
	self assert: (p1 extensionSelectorsForClass: Object) isEmpty.
	self assert: (p1 extensionSelectorsForClass: Object class) isEmpty
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testMetaclassHasExtensions [

	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClassName: a2  name).
	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #classSideMethodDefinedInP3 ofMetaclassName: a2 name).
	self assert: (p3 includesExtensionSelector: #classSideMethodDefinedInP3 ofClass: a2 class)

	
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testMethods [

	| m1 m3 |
	m1 := p1 methods.
	self assert: m1 size = 3. 
	self assert: (m1 includes: a1>>#methodDefinedInP1).
	self assert: (m1 includes: a1>>#anotherMethodDefinedInP1). 
	self assert: (m1 includes: a2>>#methodDefinedInP1).
	m3 := p3 methods.
	self assert: m3 size = 2.
	self assert: (m3 includes: a2>>#methodDefinedInP3).
	self assert: (m3 includes: a2 class>>#classSideMethodDefinedInP3)
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testMethodsForClass [

	self assert: (p1 methodsForClass: a1) size = 2. 
	self assert: ((p1 methodsForClass: a1) includes: ((Smalltalk at: #A1DefinedInP1)>>#methodDefinedInP1)).
	self assert: ((p1 methodsForClass: a1) includes: ((Smalltalk at: #A1DefinedInP1)>>#anotherMethodDefinedInP1)).
	self assert: (p1 methodsForClass: b1) isEmpty. 
	self assert: (p1 methodsForClass: Object) isEmpty. 
	self assert: (p1 methodsForClass: Object class) isEmpty. 

	self assert: (p3 methodsForClass: a2) size = 1.
	self assert: ((p3 methodsForClass: a2) includes: ((Smalltalk at: #A2DefinedInP2)>>#methodDefinedInP3)).
	self assert: (p3 methodsForClass: a2 class) size = 1.
	self assert:  ((p3 methodsForClass: a2 class) includes: ((Smalltalk at: #A2DefinedInP2) class>>#classSideMethodDefinedInP3))	
]

{ #category : #'test accessing' }
RPackageReadOnlyCompleteSetupTest >> testPackagesOfClass [
	"since a class can be extended by several packages, we want the complete list of packages that define or extend
	the class"
	
	| packages extending |
	packages := a2 packages.
	"a2 is extended by p1 and p3"
	extending := a2 extendingPackages.
	self assert: extending size = 2. 
	self assert: (extending includes: p1).
	self assert: (extending includes: p3).
	
	self assert: packages size = 3. 
	self assert: (packages includes: p1).
	self assert: (packages includes: p2).
	self assert: (packages includes: p3).
	
	
	
	
	
	
	
]

{ #category : #'test tag class' }
RPackageReadOnlyCompleteSetupTest >> testRemoveTag [
	
	self assert: p1 classTags isEmpty.
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 1.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 2.
	p1 removeClassTag: #bar.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 2.
	p1 removeClassTag: #foo.
	self deny: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 0
]

{ #category : #'test tag class' }
RPackageReadOnlyCompleteSetupTest >> testRemoveTaggedClasses [
	
	p1 addClassDefinitionName: #A1DefinedInP1 toClassTag: #foo.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #foo.
	p1 addClassDefinitionName: #B1DefinedInP1 toClassTag: #zork.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 2.
	self deny: (((p1 classesForClassTag: #zork) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #zork) collect: #name) includes: #B1DefinedInP1).	"now when we remove a class"	"from an existing tags list"
	p1 removeClassDefinitionName: #A1DefinedInP1 fromClassTag: #foo.
	self deny: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 1.	"from a nonexisting tag list"
	p1 removeClassDefinitionName: #B1DefinedInP1 fromClassTag: #taz.
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (((p1 classesForClassTag: #zork) collect: #name) includes: #B1DefinedInP1).	"with a class not registered to a tag list"
	p1 removeClassDefinitionName: #Tagada fromClassTag: #foo.
	self deny: (((p1 classesForClassTag: #foo) collect: #name) includes: #A1DefinedInP1).
	self assert: (((p1 classesForClassTag: #foo) collect: #name) includes: #B1DefinedInP1).
	self assert: (p1 classesForClassTag: #foo) size = 1
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testSelectors [
		
	self assert: (p1 selectors) size = 2. 
	self assert: ((p1 selectors) includes: #methodDefinedInP1).
	self assert: ((p1 selectors) includes: #anotherMethodDefinedInP1). 

	self assert: p3 selectors size = 2.
	self assert: (p3 selectors includes: #methodDefinedInP3).
	self assert:  (p3 selectors includes: #classSideMethodDefinedInP3)	
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testSelectorsForClass [
		
	self assert: (p1 selectorsForClass: a1) size = 2. 
	self assert: ((p1 selectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((p1 selectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assert: (p1 selectorsForClass: b1) isEmpty. 
	self assert: (p1 selectorsForClass: Object) isEmpty. 
	self assert: (p1 selectorsForClass: Object class) isEmpty. 

	self assert: (p3 selectorsForClass: a2) size = 1.
	self assert: ((p3 selectorsForClass: a2) includes: #methodDefinedInP3).
	self assert: (p3 selectorsForClass: a2 class) size = 1.
	self assert:  ((p3 selectorsForClass: a2 class) includes: #classSideMethodDefinedInP3)	
]

{ #category : #'test situation' }
RPackageReadOnlyCompleteSetupTest >> testStartingSituation [	

 	self deny: (p2 includesClass: b1).
	self assert: (p2 includesClass: b2).
	"a locally defined class not extended by other packages"
	
	self assert: (p2 includesClass: a2).  
	"a locally defined class extended by other packages"
	
	self assert: (p1 definesOrExtendsClass: a2).
	self deny: (p1 includesClass: a2).

]
