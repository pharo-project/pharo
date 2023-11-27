"
The tests takes as fixture the following situation and exercises the readonly queries.
We should be able to use a test resources to speed it up.

P1 
	A1DefinedInX
	A1DefinedInX>>methodDefinedInP1
	B1DefinedInX	
	A2DefinedInY>>methodDefinedInP1
	
P2
	A2DefinedInY
	A2DefinedInY>>methodDefinedInP2
	B2DefinedInB2	

P3
	A3DefinedInZ	
	A2DefinedInY>>methodDefinedInP3 
"
Class {
	#name : 'PackageOnModelTest',
	#superclass : 'PackageTestCase',
	#instVars : [
		'a1',
		'b1',
		'a2',
		'b2',
		'xPackage',
		'yPackage',
		'zPackage'
	],
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'running' }
PackageOnModelTest >> setUp [

	super setUp.
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.

	a1 := self newClassNamed: #A1DefinedInX in: xPackage.
	b1 := self newClassNamed: #B1DefinedInX in: xPackage.
	a2 := self newClassNamed: #A2DefinedInY in: yPackage.
	b2 := self newClassNamed: #B2DefinedInY in: yPackage.

	a1 compile: 'methodDefinedInP1 ^ #methodDefinedInP1'.
	a1 compile: 'anotherMethodDefinedInP1 ^ #anotherMethodDefinedInP1'.

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.

	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	a2 class compile: 'classSideMethodDefinedInP3 ^ #classSideMethodDefinedInP3' classified: '*' , zPackage name
]

{ #category : 'tests - tag class' }
PackageOnModelTest >> testAddTag [

	self assert: xPackage tags size equals: 1. "We start with the root tag"
	xPackage ensureTag: #baz.
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: xPackage tags size equals: 2.

	xPackage moveClass: a1 toTag: #foo.
	xPackage moveClass: b1 toTag: #foo.
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: xPackage tags size equals: 2. "foo and baz. The root tag got automatically removed since it was empty."
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (xPackage classesTaggedWith: #foo) size equals: 2.

	xPackage ensureTag: #foo.
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (xPackage classesTaggedWith: #foo) size equals: 2
]

{ #category : 'tests - tag class' }
PackageOnModelTest >> testAddTagNames [

	self assert: xPackage tags size equals: 1. "We start with the root tag"
	xPackage ensureTag: #baz.
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: xPackage tags size equals: 2.

	xPackage moveClass: a1 toTag: #foo.
	xPackage moveClass: b1 toTag: #foo.
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: xPackage tags size equals: 2. "foo and baz. The root tag got automatically removed since it was empty."
	self assert: ((xPackage tagNamed: #foo) classNames includes: #A1DefinedInX).
	self assert: ((xPackage tagNamed: #foo) classNames includes: #B1DefinedInX).
	self assert: (xPackage tagNamed: #foo) classNames size equals: 2.

	xPackage ensureTag: #foo.
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #baz ]).
	self assert: (xPackage tags anySatisfy: [ :tag | tag name = #foo ]).
	self assert: ((xPackage tagNamed: #foo) classNames includes: #A1DefinedInX).
	self assert: ((xPackage tagNamed: #foo) classNames includes: #B1DefinedInX).
	self assert: (xPackage tagNamed: #foo) classNames size equals: 2
]

{ #category : 'tests - tag class' }
PackageOnModelTest >> testAddTagsToAClass [

	self assert: xPackage tags size equals: 1. "We start with the root tag"

	xPackage moveClass: a1 toTag: #foo.
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self assert: (xPackage classesTaggedWith: #foo) size equals: 1.

	xPackage moveClass: b1 toTag: #foo.
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (xPackage classesTaggedWith: #foo) size equals: 2.

	xPackage moveClass: b1 toTag: #zork.
	self assert: (((xPackage classesTaggedWith: #zork) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (xPackage classesTaggedWith: #foo) size equals: 1.
	self assert: (xPackage classesTaggedWith: #zork) size equals: 1
]

{ #category : 'tests - compiled method' }
PackageOnModelTest >> testClassIsExtendedInPackage [

	self deny: (a1 isExtendedInPackage: xPackage).
	self assert: (xPackage includesClass: a1).
	self deny: (xPackage extendsClass: a1).
	self assert: (a2 isExtendedInPackage: xPackage).
	self deny: (xPackage includesClass: a2).
	self assert: (xPackage extendsClass: a2)
]

{ #category : 'tests - compiled method' }
PackageOnModelTest >> testCompiledMethodPackage [

	self assert: (a1 >> #methodDefinedInP1) package equals: xPackage.
	self assert: (a2 >> #methodDefinedInP1) package equals: xPackage
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testDefinedSelectorsForClass [

	self assert: (xPackage definedSelectorsForClass: a1) size equals: 2.
	self assert: (xPackage definedMethodsForClass: a1) size equals: 2.
	self assert: ((xPackage definedSelectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((xPackage definedSelectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assert: ((xPackage definedMethodsForClass: a1) includes: a1 >> #methodDefinedInP1).
	self assertEmpty: (xPackage definedSelectorsForClass: Object).
	self assertEmpty: (xPackage definedSelectorsForClass: Object class)
]

{ #category : 'tests - tag class' }
PackageOnModelTest >> testEmpty [

	self assertEmpty: (Package named: 'new package') tags
]

{ #category : 'tests - accessing' }
PackageOnModelTest >> testExtendingPackagesOfClass [
	"since a class can be extended by several packages, we want the list of packages that extend
	the class"

	| packages |
	packages := a2 extendingPackages.
	"a2 is extended by p1 and p3"
	self assert: packages size equals: 2.
	self assert: (packages includes: xPackage).
	self deny: (packages includes: yPackage).
	self assert: (packages includes: zPackage).

	packages := a1 extendingPackages.
	self assertEmpty: packages
]

{ #category : 'tests - accessing' }
PackageOnModelTest >> testExtensionMethods [
	"a package can extend several classes, either the class or  the meta-class side. 'extensionMethods' should list all the methods involved in shuch extensions. P3 extend a2 and a2 class"

	self assert: (zPackage extensionMethods includes: a2 >> #methodDefinedInP3).
	self assert: (zPackage extensionMethods includes: a2 class >> #classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testExtensionSelectors [
	self assertEmpty: a1 extensionSelectors.

	self assert: a2 extensionSelectors size equals: 2.
	self assert: (a2 extensionSelectors includes: #methodDefinedInP1).
	self assert: (a2 extensionSelectors includes: #methodDefinedInP3)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testExtensionSelectorsForClass [

	self assert: (xPackage extensionSelectorsForClass: a2) size equals: 1.
	self assert: ((xPackage extensionSelectorsForClass: a2) includes: #methodDefinedInP1).
	self assert: ((xPackage extensionMethodsForClass: a2) includes: a2 >> #methodDefinedInP1).
	self assertEmpty: (xPackage extensionSelectorsForClass: Object).
	self assertEmpty: (xPackage extensionSelectorsForClass: Object class)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testMetaclassHasExtensions [

	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #classSideMethodDefinedInP3 ofClass: a2 class)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testMethods [

	| m1 m3 |
	m1 := xPackage methods.
	self assert: m1 size equals: 3.
	self assert: (m1 includes: a1>>#methodDefinedInP1).
	self assert: (m1 includes: a1>>#anotherMethodDefinedInP1).
	self assert: (m1 includes: a2>>#methodDefinedInP1).
	m3 := zPackage methods.
	self assert: m3 size equals: 2.
	self assert: (m3 includes: a2>>#methodDefinedInP3).
	self assert: (m3 includes: a2 class>>#classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testMethodsForClass [

	self assert: (xPackage methodsForClass: a1) size equals: 2.
	self assert: ((xPackage methodsForClass: a1) includes: a1 >> #methodDefinedInP1).
	self assert: ((xPackage methodsForClass: a1) includes: a1 >> #anotherMethodDefinedInP1).
	self assertEmpty: (xPackage methodsForClass: b1).
	self assertEmpty: (xPackage methodsForClass: Object).
	self assertEmpty: (xPackage methodsForClass: Object class).

	self assert: (zPackage methodsForClass: a2) size equals: 1.
	self assert: ((zPackage methodsForClass: a2) includes: a2 >> #methodDefinedInP3).
	self assert: (zPackage methodsForClass: a2 class) size equals: 1.
	self assert: ((zPackage methodsForClass: a2 class) includes: a2 class >> #classSideMethodDefinedInP3)
]

{ #category : 'tests - accessing' }
PackageOnModelTest >> testPackagesOfClass [
	"since a class can be extended by several packages, we want the complete list of packages that define or extend
	the class"

	| packages extending |
	packages := a2 packages.
	"a2 is extended by p1 and p3"
	extending := a2 extendingPackages.
	self assert: extending size equals: 2.
	self assert: (extending includes: xPackage).
	self assert: (extending includes: zPackage).

	self assert: packages size equals: 3.
	self assert: (packages includes: xPackage).
	self assert: (packages includes: yPackage).
	self assert: (packages includes: zPackage)
]

{ #category : 'tests - tag class' }
PackageOnModelTest >> testRemoveTaggedClasses [

	xPackage moveClass: a1 toTag: #foo.
	xPackage moveClass: b1 toTag: #foo.
	xPackage moveClass: b1 toTag: #zork.
	self assert: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self deny: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assert: (xPackage classesTaggedWith: #foo) size equals: 1.
	self deny: (((xPackage classesTaggedWith: #zork) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self assert: (((xPackage classesTaggedWith: #zork) collect: [ :each | each name ]) includes: #B1DefinedInX).

	"now when we remove a class" "from an existing tags list"
	xPackage removeClass: a1.
	self deny: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self deny: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assertEmpty: (xPackage classesTaggedWith: #foo).

	"with a class not registered to a tag list"
	xPackage removeClass: self class.
	self deny: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #A1DefinedInX).
	self deny: (((xPackage classesTaggedWith: #foo) collect: [ :each | each name ]) includes: #B1DefinedInX).
	self assertEmpty: (xPackage classesTaggedWith: #foo)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testSelectors [

	self assert: xPackage selectors size equals: 2.
	self assert: (xPackage selectors includes: #methodDefinedInP1).
	self assert: (xPackage selectors includes: #anotherMethodDefinedInP1).

	self assert: zPackage selectors size equals: 2.
	self assert: (zPackage selectors includes: #methodDefinedInP3).
	self assert: (zPackage selectors includes: #classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testSelectorsForClass [

	self assert: (xPackage selectorsForClass: a1) size equals: 2.
	self assert: ((xPackage selectorsForClass: a1) includes: #methodDefinedInP1).
	self assert: ((xPackage selectorsForClass: a1) includes: #anotherMethodDefinedInP1).
	self assertEmpty: (xPackage selectorsForClass: b1).
	self assertEmpty: (xPackage selectorsForClass: Object).
	self assertEmpty: (xPackage selectorsForClass: Object class).

	self assert: (zPackage selectorsForClass: a2) size equals: 1.
	self assert: ((zPackage selectorsForClass: a2) includes: #methodDefinedInP3).
	self assert: (zPackage selectorsForClass: a2 class) size equals: 1.
	self assert: ((zPackage selectorsForClass: a2 class) includes: #classSideMethodDefinedInP3)
]

{ #category : 'tests - situation' }
PackageOnModelTest >> testStartingSituation [

	self deny: (yPackage includesClass: b1).
	self assert: (yPackage includesClass: b2).
	"a locally defined class not extended by other packages"

	self assert: (yPackage includesClass: a2).
	"a locally defined class extended by other packages"

	self assert: (xPackage definesOrExtendsClass: a2).
	self deny: (xPackage includesClass: a2)
]
