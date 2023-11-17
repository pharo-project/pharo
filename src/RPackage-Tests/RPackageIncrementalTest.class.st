"
The tests takes as fixture the following situation (which is constructed partially)
For tests that do not need to check the incremental construction.


P1 
	A1DefinedInP1>>methodDefinedInP1
	B1DefinedInP1	
	A2DefinedInP2>>methodDefinedInP1  (was *P2)
	
P2
	A2DefinedInP2>>methodDefinedInP2
	B2DefinedInB2	

P3
	A3DefinedInP3	
	A2DefinedInP2>>methodDefinedInP3  (was *P2*)
"
Class {
	#name : 'RPackageIncrementalTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'utilities' }
RPackageIncrementalTest >> removeClassNamedIfExists: aClassNameSymbol [
	testingEnvironment at: aClassNameSymbol asSymbol ifPresent: [:c| c removeFromSystem]
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testAddClassNoDuplicate [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPackageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	b1 := self newClassNamed: #B1InPackageP1 in: self yPackageName.
	xPackage addClass: a1.
	"adding the same class does not do anything - luckily"
	self assert: xPackage definedClasses size equals: 1.
	xPackage addClass: b1.
	self assert: xPackage definedClasses size equals: 2
]

{ #category : 'tests - method addition removal' }
RPackageIncrementalTest >> testAddRemoveMethod [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.

	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	self assert: (a2 >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self deny: (zPackage includesSelector: #methodDefinedInP1 ofClass: a2).

	a2 removeSelector: #methodDefinedInP2.
	self deny: (yPackage includesSelector: #methodDefinedInP2 ofClass: a2).

	a2 removeSelector: #methodDefinedInP1.
	self deny: (xPackage includesSelector: #methodDefinedInP3 ofClass: a2)
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testClassAddition [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPAckageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: a1 class)
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testClassDefinitionRemoval [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPAckageP1 in: self yPackageName.
	b1 := self newClassNamed: #B1InPAckageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.

	xPackage addClass: a1.
	xPackage addClass: b1.
	self assert: xPackage definedClasses size equals: 2.

	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	self deny: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: b1.
	self deny: (xPackage includesClass: b1)
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testClassDefinitionWithTagsRemoval [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.

	a1 := self newClassNamed: #A1InPAckageP1 in: xPackage.
	b1 := self newClassNamed: #B1InPAckageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 2.

	xPackage moveClass: a1 toTag: 'a1-tag'.
	xPackage moveClass: b1 toTag: 'b1-tag'.
	self assert: xPackage classTags size equals: 2.

	self assert: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	self deny: (xPackage includesClass: a1).
	self assert: (xPackage includesClass: b1).

	xPackage removeClass: b1.
	self deny: (xPackage includesClass: b1).
	self assert: xPackage classTags size equals: 0
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testDefinedClassesAndDefinedClassNames [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 1.
	self assert: (xPackage definedClasses includes: a1).
	self assert: (xPackage definedClassNames includes: a1 name).

	b1 := self newClassNamed: #B1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 2.
	self assert: (xPackage definedClasses includes: b1).
	self assert: (xPackage definedClassNames includes: b1 name)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testExtensionClassNames [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: b2).
	self assert: (yPackage includesClass: a2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 1.
	self assert: xPackage extensionMethods size equals: 1.
	self assert: (xPackage extendedClassNames includes: #A2InPackageP2).
	self deny: (xPackage includesClass: a2). "method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 2.
	self assert: xPackage extensionMethods size equals: 2.
	self assert: (xPackage extendedClassNames includes: #B2InPackageP2).
	self deny: (xPackage includesClass: b2).

	b2 compile: 'secondMethodInB2PackagedInP1 ^ 2' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 3.
	self assert: xPackage extensionMethods size equals: 3.
	self assert: xPackage extendedClasses size equals: 2.
	self assert: (xPackage extendedClassNames includes: #B2InPackageP2).
	self deny: (xPackage includesClass: b2)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testExtensionClasses [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: a2).

	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage extendedClasses size equals: 1.
	self assert: (xPackage extendedClasses includes: a2).
	self assert: xPackage extendedClassNames size equals: 1.
	self assert: (xPackage extendedClassNames includes: a2 name).

	b2 class compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage extendedClasses size equals: 2.
	self assert: (xPackage extendedClasses includes: b2).
	"extensionClasses returns or metaclasses while extensionClassNames returns class names (but not metaclass names)"

	self assert: xPackage extendedClassNames size equals: 2.
	self assert: (xPackage extendedClassNames includes: b2 name)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testExtensionClassesWithCompiledMethod [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: b2).
	self assert: (yPackage includesClass: b2).

	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionMethods size equals: 1.
	self assert: xPackage extensionSelectors size equals: 1.
	self assert: xPackage extendedClasses size equals: 1.
	self deny: (xPackage includesClass: a2).
	"method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 2.
	self assert: xPackage extensionMethods size equals: 2.
	self assert: xPackage extendedClasses size equals: 2.
	self deny: (xPackage includesClass: b2).

	b2 compile: 'secondMethodInB2PackagedInP1 ^ 2' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 3.
	self assert: xPackage extensionMethods size equals: 3.
	self assert: xPackage extendedClasses size equals: 2.
	self deny: (xPackage includesClass: b2)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testExtensionMethods [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionSelectors size equals: 1.
	self deny: (xPackage includesClass: a2).
	"method extension class are not included in packages"

	b2 compile: 'firstMethodInB2PackagedInP1 ^ 1' classified: '*' , xPackage name.
	self assert: xPackage extensionSelectors size equals: 2
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testIncludeClass [

	| xPackage yPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self deny: (xPackage includesClass: a2).
	xPackage addMethod: a2 >> #methodPackagedInP1.
	self deny: (xPackage includesClass: a2).
	"We should declare the class explictly. Adding a method does not declare
	the class as defined. The reason is that like that the client controls the granularity
	and moment of class registration."

	xPackage addClass: a2.
	self assert: (xPackage includesClass: a2).
	self assert: (xPackage includesClassNamed: a2 name)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testIncludeClassMore [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	self assert: (yPackage includesClass: a2).
	self deny: (xPackage includesClass: a2).
	self deny: (zPackage includesClass: a2)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testIncludeSelectorOfClass [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	"includesSelector checks both in defined and extension so we test both"
	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a2).
	self assert: (a2 >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a2).

	self deny: (yPackage includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).

	self deny: (yPackage includesSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testIncludeSelectorOfMetaClass [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.

	a2 class compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 class compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 class compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	"includesSelector checks both in defined and extension so we test both"
	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a2 class).
	self assert: (a2 class >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 class >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a2 class).

	self deny: (yPackage includesSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2 class).

	self deny: (yPackage includesSelector: #methodDefinedInP1 ofClass: a2 class).
	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2 class)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testIncludesMethodOfClassInPresenceOfOtherPackageExtensions [

	| xPackage yPackage zPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.

	a2 compile: 'methodDefinedInP2 ^ #methodDefinedInP2'.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.
	a2 compile: 'methodDefinedInP3 ^ #methodDefinedInP3' classified: '*' , zPackage name.

	self assert: (xPackage includesExtensionSelector: #methodDefinedInP1 ofClass: a2).
	self assert: (zPackage includesExtensionSelector: #methodDefinedInP3 ofClass: a2).
	self assert: (a2 >> #methodDefinedInP2) package equals: yPackage.
	self deny: (a2 >> #methodDefinedInP2) isExtension.
	self deny: (yPackage includesSelector: #methodDefinedInP3 ofClass: a2).
	self deny: (yPackage includesSelector: #methodDefinedInP1 ofClass: a2)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testIncludesOrTouches [

	| xPackage yPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	self deny: (xPackage includesClass: a2).
	self assert: (yPackage includesClass: a2).

	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage extensionMethods size equals: 1.
	self assert: xPackage extensionSelectors size equals: 1.
	self deny: (xPackage includesClass: a2).
	self assert: (xPackage definesOrExtendsClass: a2).
	self assert: (yPackage includesClass: a2)
]

{ #category : 'tests - method addition removal' }
RPackageIncrementalTest >> testMethodAddition [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1DefinedInP1 in: xPackage.
	a1 compileSilently: 'foo ^ 10'.
	xPackage addMethod: a1 >> #foo.
	self assert: (xPackage includesSelector: #foo ofClass: a1)
]

{ #category : 'tests - method addition removal' }
RPackageIncrementalTest >> testMethodPackageResolution [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A2InPackageP1 in: xPackage.
	a1 compile: 'method ^ #methodDefinedInP1'.
	a1 class compile: 'method ^ #methodDefinedInP1'.

	self assert: (a1 >> #method) package identicalTo: xPackage.
	self assert: (a1 class >> #method) package identicalTo: xPackage
]

{ #category : 'tests - package belonging' }
RPackageIncrementalTest >> testPackageOfClassForClassesNotDefinedInPackageButJustExtendingIt [

	| xPackage yPackage a2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	a2 compile: 'methodDefinedInP1 ^ #methodDefinedInP1' classified: '*' , xPackage name.

	self assert: a2 package equals: yPackage.
	xPackage extensionMethods do: [ :each | "the package of a class which is extended inside a package p, is not p
	but the package where the class was defined"
		self deny: each methodClass package equals: xPackage ]
]

{ #category : 'tests - package belonging' }
RPackageIncrementalTest >> testPackageOfClassForDefinedClasses [

	| xPackage a1 b1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPAckageP1 in: xPackage.
	b1 := self newClassNamed: #B1InPAckageP1 in: xPackage.

	self assert: a1 package equals: xPackage.
	self assert: b1 package equals: xPackage
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testRemoveClassRemovesExtensions [

	| xPackage yPackage a1 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	"the class is created but not added to the package for now"
	a1 := self newClassNamed: #A1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 1.
	a1 compile: 'methodDefinedInP2 ^ #methodDefinedInP2' classified: '*' , yPackage name.

	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1).
	self assert: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a1).

	a1 removeFromSystem.

	self deny: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1)
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testRemoveExtensionMethodRemovesExtensionsFromPackage [

	| xPackage yPackage a1 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	"the class is created but not added to the package for now"
	a1 := self newClassNamed: #A1InPackageP1 in: xPackage.
	self assert: xPackage definedClasses size equals: 1.
	a1 compile: 'methodDefinedInP2 ^ #methodDefinedInP2' classified: '*' , yPackage name.

	self assert: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1).
	self assert: (yPackage includesExtensionSelector: #methodDefinedInP2 ofClass: a1).

	a1 removeSelector: #methodDefinedInP2.

	self deny: (yPackage includesSelector: #methodDefinedInP2 ofClass: a1)
]

{ #category : 'tests - extension' }
RPackageIncrementalTest >> testTwoClassesWithExtensions [

	| xPackage yPackage a2 b2 |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.

	a2 := self newClassNamed: #A2InPackageP2 in: yPackage.
	b2 := self newClassNamed: #B2InPackageP2 in: yPackage.
	a2 compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.
	b2 class compile: 'methodPackagedInP1 ^ #methodPackagedInP1' classified: '*' , xPackage name.

	self assert: xPackage classes size equals: 2.
	self assert: yPackage classes size equals: 2
]

{ #category : 'tests - class addition removal' }
RPackageIncrementalTest >> testUniqueClassInDefinedClassesUsingAddClassDefinition [

	| xPackage a1 |
	xPackage := self ensureXPackage.
	a1 := self newClassNamed: #A1InPackageP1 in: self yPackageName.
	self assertEmpty: xPackage definedClasses.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	xPackage addClass: a1.
	self assert: xPackage definedClasses size equals: 1.
	xPackage addClass: a1 class.
	self assert: xPackage definedClasses size equals: 1
]
