"
Abstract Test of all the TraitsV2 tests
"
Class {
	#name : #T2AbstractTest,
	#superclass : #TestCase,
	#instVars : [
		'createdClasses'
	],
	#category : #'TraitsV2-Tests'
}

{ #category : #'instance creation' }
T2AbstractTest >> newClass: aName superclass: aSuperclass with: slots uses: aComposition [
	
	^ self newClass: aName superclass: aSuperclass with: slots uses: aComposition category: 'TraitsV2-Tests-TestClasses' 
]

{ #category : #'instance creation' }
T2AbstractTest >> newClass: aName superclass: aSuperclass with: slots uses: aComposition category: category [
	| class |
	class := aSuperclass subclass: aName uses: aComposition slots: slots classVariables: #() poolDictionaries: '' category: category. 
	createdClasses add:class.
	
	^class.
]

{ #category : #'instance creation' }
T2AbstractTest >> newClass: aName with: slots uses: aComposition [
	^ self newClass: aName superclass: Object with: slots uses: aComposition
]

{ #category : #'instance creation' }
T2AbstractTest >> newTrait: aName with: slots [
	^ self newTrait: aName with: slots uses: {}
]

{ #category : #'instance creation' }
T2AbstractTest >> newTrait: aName with: slots uses: aComposition [
	^ self newTrait: aName with: slots uses: aComposition category: 'TraitsV2-Tests-TestClasses'
]

{ #category : #'instance creation' }
T2AbstractTest >> newTrait: aName with: slots uses: aComposition category: category [
	| class |
	class := Trait named: aName uses: aComposition slots: slots category: category  env: self class environment. 
	createdClasses add:class.
	
	^class.
]

{ #category : #running }
T2AbstractTest >> setUp [
	super setUp.
	createdClasses := OrderedCollection new.
]

{ #category : #running }
T2AbstractTest >> tearDown [
	createdClasses reverse do:#removeFromSystem.
	createdClasses := nil.
	
	super tearDown.
]
