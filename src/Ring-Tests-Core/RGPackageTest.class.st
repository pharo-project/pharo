"
SUnit tests for packages
"
Class {
	#name : #RGPackageTest,
	#superclass : #RGTest,
	#category : #'Ring-Tests-Core'
}

{ #category : #testing }
RGPackageTest >> testAddingClass [

	| newPackage newClass env |
	
	newPackage := RGPackage named: 'Collections-Sequenceable'.
	env := newPackage environment.
	newClass:= RGClass named: #OrderedCollection parent: env.
	
	self assert: (env ask packages notEmpty).
	
	newPackage addDefinedBehavior: newClass.

]

{ #category : #testing }
RGPackageTest >> testAddingPackage [
"	| newPackage childPackage |
	
	newPackage:= RGPackage named: #'Collections'.
	newPackage addPackageNamed: #'Collections-Sequenceable'.
		
	self assert: (newPackage includesPackageNamed: #'Collections-Sequenceable').
	self assert: newPackage packages size equals: 1.
	
	childPackage:= newPackage packageNamed: #'Collections-Sequenceable'.
	self assert: (childPackage parent = newPackage parent).
	self assert: (childPackage package = newPackage)."
]

{ #category : #testing }
RGPackageTest >> testCategories [
	| package env class1 class2 |
	package := RGPackage named: 'SomePackage'.
	env := package environment.

	class1 := env ensureClassNamed: #Class1.
	class2 := env ensureClassNamed: #Class2.

	class1 package: package.
	class2 package: package.

	self assert: package categories asArray equals: #('SomePackage').

	class1 tagWith: #tag1.
	class2 tagWith: #tag1.

	self assert: package categories asArray sorted equals: #('SomePackage-tag1').

	class2 untagFrom: #tag1.
	class2 tagWith: #tag2.

	self assert: package categories asArray sorted equals: #('SomePackage-tag1' 'SomePackage-tag2')
]

{ #category : #testing }
RGPackageTest >> testClasses [
	
	| package env class1 class2 trait1 |

	package := RGPackage named: 'SomePackage'.
	env := package environment.
	
	self assert: package classes isEmpty.
	self assert: package definedClasses isEmpty.
	self assert: package definedTraits isEmpty.
	self assert: package classNames isEmpty.
	self assert: package extendedBehaviors isEmpty.
	self assert: package extendedClasses isEmpty.
	self assert: package extendedClassesAndTraits isEmpty.
	self assert: package extendedSelectors isEmpty.
	self assert: package extendedTraits isEmpty.
		
	class1 := env ensureClassNamed: #Class1.
	class2 := env ensureClassNamed: #Class2.
	
	class1 package: package.
	class2 package: package.
	
	self assert: package classes size equals: 2.
	self assert: (package classes includes: class1).
	self assert: (package classes includes: class2).
	
	self assert: package definedClasses size equals: 2.
	self assert: (package definedClasses includes: class1).
	self assert: (package definedClasses includes: class2).

	self assert: package classNames asArray sorted equals: #(Class1 Class2).

	trait1 := env ensureTraitNamed: #Trait1.
	trait1 package: package.

	self assert: package definedClasses size equals: 3.
	self assert: (package definedClasses includes: trait1).
	self assert: package definedClassesOnly size equals: 2.
	self assert: (package definedClassesOnly allSatisfy: #isClass).

	self assert: package definedTraits size equals: 1.
	self assert: (package definedClasses includes: trait1).

]

{ #category : #testing }
RGPackageTest >> testDefinedBehaviorsCollection [
	
	| package env behavior1 behavior2 behavior3 |

	package := RGPackage unnamed.
	env := package environment.

	self assert: package definedBehaviors isEmpty.
	self assert: (package hasUnresolved: #definedBehaviors).
	
	behavior1 := env ensureClassNamed: #Behavior1.
	package addDefinedBehavior: behavior1.
	self assert: package definedBehaviors size equals: 1.

	self assert: (package hasResolved: #definedBehaviors).
	
	behavior2 := env ensureClassNamed: #'Behavior2 class'.
	package addDefinedBehavior: behavior2.
	self assert: package definedBehaviors size equals: 2.
	
	behavior3 := env ensureTraitNamed: #Behavior3.
	package addDefinedBehavior: behavior3.
	self assert: package definedBehaviors size equals: 3.

	package removeDefinedBehavior: behavior3.
	self assert: package definedBehaviors size equals: 2.

	package cleanDefinedBehaviors.
	self assert: package definedBehaviors isEmpty.
	self assert: (package hasResolved: #definedBehaviors).

]

{ #category : #testing }
RGPackageTest >> testDefinedBehaviorsOfUnresolvedPackages [

	| env class |
	
	env := RGEnvironment new.
	class := env ensureClassNamed: #SomeClass.
	self assert: class package definedBehaviors size equals: 1.
	self assert: class package definedBehaviors anyOne equals: class.

]

{ #category : #'tests - new' }
RGPackageTest >> testNewPackage [
	| newPackage |
	newPackage := RGPackage named: 'Collections-Sequenceable'.

	self assert: newPackage isRingResolved.

	self assert: newPackage isPackage.
	self assert: newPackage name equals: 'Collections-Sequenceable'.

	newPackage name: 'NewCollections'.
	self assert: newPackage name equals: 'NewCollections'.

	self assert: newPackage definedBehaviors isEmpty.
	self assert: newPackage extensionMethods isEmpty.
	self assert: (newPackage hasUnresolvedAll: #(#definedBehaviors #extensionMethods)).

	newPackage cleanDefinedBehaviors.

	self assert: (newPackage hasUnresolved: #extensionMethods).
	self assert: newPackage definedBehaviors isEmpty.
	self assert: newPackage extensionMethods isEmpty.

	newPackage cleanExtensionMethods.
	newPackage cleanTagsForClasses.

	self assert: newPackage unresolvedProperties isEmpty.
	self assert: newPackage definedBehaviors isEmpty.
	self assert: newPackage extensionMethods isEmpty.

	self assert: newPackage isRingResolved.
	self assert: newPackage isRingFullyResolved.
	self deny: newPackage isRingFullyUnresolved.

	self assert: newPackage package equals: newPackage.

	self assert: newPackage traits isEmpty
]

{ #category : #'tests - new' }
RGPackageTest >> testPrinting [
	| newPackage |
	
	newPackage := RGPackage named: 'Collections-Sequenceable'.
	
	self assert: newPackage printString equals: ('RGPackage(Collections-Sequenceable)').
	
]

{ #category : #testing }
RGPackageTest >> testResolvingConsistency [

	| anObject |
	anObject := RGPackage unnamed.
	self checkClassesConsistency: anObject and: anObject makeResolved
]

{ #category : #'tests - new' }
RGPackageTest >> testStubPackage [
	| env package package2 |
	env := RGEnvironment new.
	self assert: env ask packages size equals: 0.

	package := RGPackage named: 'Collections-Sequenceable'.
	env := package environment.
	self assert: env ask packages size equals: 1.

	package2 := RGPackage named: 'Collections-Sequenceable' parent: env
	"self assert: (env ask packages size = 2).
	env addPackage: package2.
	self assert: (env ask packages size = 3)."
]

{ #category : #testing }
RGPackageTest >> testTagsForClasses [
	
	| package env class1 class2 |

	package := RGPackage unnamed.
	env := package environment.
	
	self assert: package tagsForClasses isEmpty.
	self assert: (package hasUnresolved: #tagsForClasses).
	
	class1 := env ensureClassNamed: #Class1.
	class2 := env ensureClassNamed: #Class2.
	
	class1 package: package.
	class2 package: package.
	
	self assert: package tagsForClasses isEmpty.
	self assert: (package hasUnresolved: #tagsForClasses).

	self assert: (package classesTaggedWith: #tag1) isEmpty.
	
	class1 tagWith: #tag1.
	self assert: class1 tags equals: #(#tag1).
	self assert: package tagsForClasses equals: #(#tag1).
	self assert: (package classesTaggedWith: #tag1) equals: {class1}.
	
	class2 tagWith: #tag1.
	self assert: class2 tags equals: #(#tag1).
	self assert: package tagsForClasses equals: #(#tag1).
	self assert: ((package classesTaggedWith: #tag1) includesAll: {class1. class2}).

	class1 tagWith: #tag2.
	self assert: class1 tags sorted equals: #(#tag1 #tag2).
	self assert: package tagsForClasses sorted equals: #(#tag1 #tag2).
	
	class1 untagFrom: #tag1.
	self assert: class1 tags equals: #(#tag2).
	self assert: package tagsForClasses sorted equals: #(#tag1 #tag2).
	self assert: ((package classesTaggedWith: #tag1) includesAll: {class2}).

	class2 tagWith: #tag3.
	self assert: class2 tags sorted equals: #(#tag1 #tag3).
	self assert: package tagsForClasses sorted equals: #(#tag1 #tag2 #tag3).

	package removeDefinedBehavior: class1.
	self assert: package tagsForClasses sorted equals: #(#tag1 #tag2 #tag3).
	package removeDefinedBehavior: class2.
	self assert: package tagsForClasses sorted equals: #(#tag1 #tag2 #tag3).

	self assert: (package classesTaggedWith: #tag1) isEmpty.
	self assert: (package classesTaggedWith: #tag2) isEmpty.
	self assert: (package classesTaggedWith: #tag3) isEmpty.

]

{ #category : #testing }
RGPackageTest >> testTagsForClassesCollection [
	
	| package env tag1 tag2 tag3 |

	package := RGPackage unnamed.
	env := package environment.

	self assert: package tagsForClasses isEmpty.
	self assert: (package hasUnresolved: #tagsForMethods).
	
	tag1 := #tag1.
	package addClassTag: #tag1.
	self assert: package tagsForClasses size equals: 1.

	self assert: (package hasResolved: #tagsForClasses).
	
	tag2 := #tag2.
	package addClassTag: tag2.
	self assert: package tagsForClasses size equals: 2.
	
	tag3 := #tag3.
	package addClassTag: tag3.
	self assert: package tagsForClasses size equals: 3.

	package removeClassTag: tag3.
	self assert: package tagsForClasses size equals: 2.

	package cleanTagsForClasses.
	self assert: package tagsForClasses isEmpty.
	self assert: (package hasResolved: #tagsForClasses).

]
