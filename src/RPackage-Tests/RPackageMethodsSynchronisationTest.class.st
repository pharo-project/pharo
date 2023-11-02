"
SUnit tests for RPackage method synchronization
"
Class {
	#name : 'RPackageMethodsSynchronisationTest',
	#superclass : 'RPackageTestCase',
	#category : 'RPackage-Tests',
	#package : 'RPackage-Tests'
}

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyExtensionProtocolUpdatesThePackage [
	"test that when we move a method from an extension category ( begining with *) to another extending package , the method is moved from the extending package to the other extending package"

	| class xPackage yPackage zPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	zPackage := self ensureZPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: #newMethod inClass: class inCategory: '*yyyyy'.

	class classify: #newMethod under: '*zzzzz'.
	self deny: (xPackage includesSelector: #newMethod ofClass: class).
	self deny: (yPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (zPackage includesExtensionSelector: #newMethod ofClass: class).
	self assert: (class >> #newMethod) package equals: zPackage
]

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyProtocolOfMethodyDoesNothing [
	"test that when we move a method from a classic category (not begining with *) to another classic category , the packaging keeps the same"

	| class xPackage |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'old protocol'.

	"this we do"
	class classify: #stubMethod under: 'new protocol'.

	"this we check"
	self assert: (class >> #stubMethod) protocolName equals: 'new protocol'.
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (xPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyProtocolToExtensionUpdatesThePackage [
	"test that when we move a method from a classic category (not begining with *) to an extension category , the method is moved from the parent package of the class to the extending package"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'old protocol'.

	class classify: #stubMethod under: '*yyyyy'.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: yPackage.

	class classify: #stubMethod under: '*yyyyy-subcategory'.
	self deny: (xPackage includesSelector: #stubMethod ofClass: class).
	self assert: (yPackage includesExtensionSelector: #stubMethod ofClass: class).
	self assert: (class >> #stubMethod) package equals: yPackage
]

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testModifyProtocolToRemoveExtensionUpdatesThePackage [
	"test that when we move a method from an extension category ( begining with *) to a classic category , the method is moved from the extending package to the parent package of the class"

	| class xPackage yPackage |
	xPackage := self ensureXPackage.
	yPackage := self ensureYPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: '*yyyyy'.

	class classify: #stubMethod under: 'classic protocol'.
	self assert: (class >> #stubMethod) package equals: xPackage.
	self deny: (class >> #stubMethod) isExtension.
	self deny: (yPackage includesExtensionSelector: #stubMethod ofClass: class)
]

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testPackageOfNonExtensionMethodIsPackageOfTheClass [
	"test that when we add a method to a  class in a classic category (not beginning with *), this method is added to the parent package of the class"

	| xPackage class |
	xPackage := self ensureXPackage.

	class := self newClassNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'protocol'.

	self assert: (class >> #stubMethod) package equals: class package.
	self deny: (class >> #stubMethod) isExtension
]

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testPackageOfNonExtensionMethodIsPackageOfTheTrait [
	"test that when we add a method to a  trait in a classic category (*not beginning with *), this method is added to the parent package of the class"

	| xPackage trait |
	xPackage := self ensureXPackage.

	trait := self newTraitNamed: 'NewClass' in: xPackage.

	self createMethodNamed: 'stubMethod' inClass: trait inCategory: 'protocol'.

	self assert: (trait >> #stubMethod) package equals: trait package.
	self deny: (trait >> #stubMethod) isExtension
]

{ #category : 'tests - operations on methods' }
RPackageMethodsSynchronisationTest >> testRemoveMethodRemoveMethodFromItsPackage [
	"test that when we remove a method, the method is remod from the package in which the method is defined"

	| xPackage class |
	xPackage := self ensureXPackage.
	class := self newClassNamed: 'NewClass' in: xPackage.
	self createMethodNamed: 'stubMethod' inClass: class inCategory: 'classic category'.

	class removeSelector: #stubMethod.

	self deny: (xPackage includesSelector: #stubMethod ofClass: class)
]
