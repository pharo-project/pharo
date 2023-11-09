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
	#name : 'RPackageReadOnlyCompleteSetupTest',
	#superclass : 'RPackageIncrementalTest',
	#instVars : [
		'p1',
		'p2',
		'p3',
		'a1',
		'b1',
		'a2',
		'a3',
		'b2'
	],
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'running' }
RPackageReadOnlyCompleteSetupTest >> setUp [

	super setUp.
	p1 := self ensurePackage: self p1Name.
	p2 := self ensurePackage: self p2Name.
	p3 := self ensurePackage: self p3Name.

	a1 := self newClassNamed: #A1DefinedInP1 in: p1.
	b1 := self newClassNamed: #B1DefinedInP1 in: p1.
	a2 := self newClassNamed: #A2DefinedInP2 in: p2.
	b2 := self newClassNamed: #B2DefinedInP2 in: p2.
	a3 := self newClassNamed: #A3DefinedInP3 in: p3.

	a1 compile: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , p1 name.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.

	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , p3 name.

	a2 class compile: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3' classified: '*' , p3 name
]

{ #category : 'tests - tag class' }
RPackageReadOnlyCompleteSetupTest >> testAddTag [

	self assert: p1 classTags size equals: 1. "We start with the root tag"
	p1 ensureTag: #baz.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: p1 classTags size equals: 2.

	p1 moveClass: a1 toTag: #foo.
	p1 moveClass: b1 toTag: #foo.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: p1 classTags size equals: 2. "foo and baz. The root tag got automatically removed since it was empty."
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2.

	p1 ensureTag: #foo.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2
]

{ #category : 'tests - tag class' }
RPackageReadOnlyCompleteSetupTest >> testAddTagNames [

	self assert: p1 classTags size equals: 1. "We start with the root tag"
	p1 ensureTag: #baz.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: p1 classTags size equals: 2.

	p1 moveClass: a1 toTag: #foo.
	p1 moveClass: b1 toTag: #foo.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: p1 classTags size equals: 2. "foo and baz. The root tag got automatically removed since it was empty."
	self assert: ((p1 classTagNamed: #foo) classNames includes: #A1DefinedInP1).
	self assert: ((p1 classTagNamed: #foo) classNames includes: #B1DefinedInP1).
	self assert: (p1 classTagNamed: #foo) classNames size equals: 2.

	p1 ensureTag: #foo.
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (p1 classTags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: ((p1 classTagNamed: #foo) classNames includes: #A1DefinedInP1).
	self assert: ((p1 classTagNamed: #foo) classNames includes: #B1DefinedInP1).
	self assert: (p1 classTagNamed: #foo) classNames size equals: 2
]

{ #category : 'tests - tag class' }
RPackageReadOnlyCompleteSetupTest >> testAddTagsToAClass [

	self assert: p1 classTags size equals: 1. "We start with the root tag"

	p1 moveClass: a1 toTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.

	p1 moveClass: b1 toTag: #foo.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 2.

	p1 moveClass: b1 toTag: #zork.
	self assert: (((p1 classesTaggedWith: #zork) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.
	self assert: (p1 classesTaggedWith: #zork) size equals: 1
]

{ #category : 'tests - compiled method' }
RPackageReadOnlyCompleteSetupTest >> testClassIsExtendedInPackage [

	self deny: (a1 isExtendedInPackage: p1).
	self assert: (p1 includesClass: a1).
	self deny: (p1 extendsClass: a1).
	self assert: (a2 isExtendedInPackage: p1).
	self deny: (p1 includesClass: a2).
	self assert: (p1 extendsClass: a2)
]

{ #category : 'tests - compiled method' }
RPackageReadOnlyCompleteSetupTest >> testCompiledMethodPackage [

	self assert: (a1 >> #methodDefinedInP1) package equals: p1.
	self assert: (a2 >> #methodDefinedInP1) package equals: p1
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testDefinedSelectorsForClass [

	self assert: (p1 definedSelectorsForClass: a1) size equals: 2.
	self assert: (p1 definedMethodsForClass: a1) size equals: 2.
	self assert: ((p1 definedSelectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((p1 definedSelectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assert: ((p1 definedMethodsForClass: a1) includes: a1 >> #methodDefinedInP1).
	self assertEmpty: (p1 definedSelectorsForClass: Object).
	self assertEmpty: (p1 definedSelectorsForClass: Object class)
]

{ #category : 'tests - tag class' }
RPackageReadOnlyCompleteSetupTest >> testEmpty [

	self assertEmpty: (RPackage named: 'new package') classTags
]

{ #category : 'tests - accessing' }
RPackageReadOnlyCompleteSetupTest >> testExtendingPackagesOfClass [
	"since a class can be extended by several packages, we want the list of packages that extend
	the class"

	| packages |
	packages := a2 extendingPackages.
	"a2 is extended by p1 and p3"
	self assert: packages size equals: 2.
	self assert: (packages includes: p1).
	self deny: (packages includes: p2).
	self assert: (packages includes: p3).

	packages := a1 extendingPackages.
	self assertEmpty: packages
]

{ #category : 'tests - accessing' }
RPackageReadOnlyCompleteSetupTest >> testExtensionMethods [
	"a package can extend several classes, either the class or  the meta-class side. 'extensionMethods' should list all the methods involved in shuch extensions. P3 extend a2 and a2 class"

	self assert: (p3 extensionMethods includes: a2 >> #methodDefinedInP3).
	self assert: (p3 extensionMethods includes: a2 class >> #classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testExtensionSelectors [
	self assertEmpty: a1 extensionSelectors.

	self assert: a2 extensionSelectors size equals: 2.
	self assert: (a2 extensionSelectors includes: #methodDefinedInP1).
	self assert: (a2 extensionSelectors includes: #methodDefinedInP3)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testExtensionSelectorsForClass [

	self assert: (p1 extensionSelectorsForClass: a2) size equals: 1.
	self assert: ((p1 extensionSelectorsForClass: a2) includes: #methodDefinedInP1).
	self assert: ((p1 extensionMethodsForClass: a2) includes: a2 >> #methodDefinedInP1).
	self assertEmpty: (p1 extensionSelectorsForClass: Object).
	self assertEmpty: (p1 extensionSelectorsForClass: Object class)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testMetaclassHasExtensions [

	self assert: (p3 includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (p3 includesExtensionSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testMethods [

	| m1 m3 |
	m1 := p1 methods.
	self assert: m1 size equals: 3.
	self assert: (m1 includes: a1>>#methodDefinedInP1).
	self assert: (m1 includes: a1>>#anotherMethodDefinedInP1).
	self assert: (m1 includes: a2>>#methodDefinedInP1).
	m3 := p3 methods.
	self assert: m3 size equals: 2.
	self assert: (m3 includes: a2>>#methodDefinedInP3).
	self assert: (m3 includes: a2 class>>#classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testMethodsForClass [

	self assert: (p1 methodsForClass: a1) size equals: 2.
	self assert: ((p1 methodsForClass: a1) includes: a1 >> #methodDefinedInP1).
	self assert: ((p1 methodsForClass: a1) includes: a1 >> #anotherMethodDefinedInP1).
	self assertEmpty: (p1 methodsForClass: b1).
	self assertEmpty: (p1 methodsForClass: Object).
	self assertEmpty: (p1 methodsForClass: Object class).

	self assert: (p3 methodsForClass: a2) size equals: 1.
	self assert: ((p3 methodsForClass: a2) includes: a2 >> #methodDefinedInP3).
	self assert: (p3 methodsForClass: a2 class) size equals: 1.
	self assert: ((p3 methodsForClass: a2 class) includes: a2 class >> #classSideMethodDefinedInP3)
]

{ #category : 'tests - accessing' }
RPackageReadOnlyCompleteSetupTest >> testPackagesOfClass [
	"since a class can be extended by several packages, we want the complete list of packages that define or extend
	the class"

	| packages extending |
	packages := a2 packages.
	"a2 is extended by p1 and p3"
	extending := a2 extendingPackages.
	self assert: extending size equals: 2.
	self assert: (extending includes: p1).
	self assert: (extending includes: p3).

	self assert: packages size equals: 3.
	self assert: (packages includes: p1).
	self assert: (packages includes: p2).
	self assert: (packages includes: p3)
]

{ #category : 'tests - tag class' }
RPackageReadOnlyCompleteSetupTest >> testRemoveTaggedClasses [

	p1 moveClass: a1 toTag: #foo.
	p1 moveClass: b1 toTag: #foo.
	p1 moveClass: b1 toTag: #zork.
	self assert: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assert: (p1 classesTaggedWith: #foo) size equals: 1.
	self deny: (((p1 classesTaggedWith: #zork) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self assert: (((p1 classesTaggedWith: #zork) collect: [ :each | each name ]) includes: #B1DefinedInP1).

	"now when we remove a class" "from an existing tags list"
	p1 removeClass: a1.
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assertEmpty: (p1 classesTaggedWith: #foo).

	"with a class not registered to a tag list"
	p1 removeClass: self class.
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInP1).
	self deny: (((p1 classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInP1).
	self assertEmpty: (p1 classesTaggedWith: #foo)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testSelectors [

	self assert: p1 selectors size equals: 2.
	self assert: (p1 selectors includes: #methodDefinedInP1).
	self assert: (p1 selectors includes: #anotherMethodDefinedInP1).

	self assert: p3 selectors size equals: 2.
	self assert: (p3 selectors includes: #methodDefinedInP3).
	self assert: (p3 selectors includes: #classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testSelectorsForClass [

	self assert: (p1 selectorsForClass: a1) size equals: 2.
	self assert: ((p1 selectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((p1 selectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assertEmpty: (p1 selectorsForClass: b1).
	self assertEmpty: (p1 selectorsForClass: Object).
	self assertEmpty: (p1 selectorsForClass: Object class).

	self assert: (p3 selectorsForClass: a2) size equals: 1.
	self assert: ((p3 selectorsForClass: a2) includes: #methodDefinedInP3).
	self assert: (p3 selectorsForClass: a2 class) size equals: 1.
	self assert: ((p3 selectorsForClass: a2 class) includes: #classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
RPackageReadOnlyCompleteSetupTest >> testStartingSituation [

	self deny: (p2 includesClass: b1).
	self assert: (p2 includesClass: b2).
	"a locally defined class not extended by other packages"

	self assert: (p2 includesClass: a2).
	"a locally defined class extended by other packages"

	self assert: (p1 definesOrExtendsClass: a2).
	self deny: (p1 includesClass: a2)
]
