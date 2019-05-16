"
I'm a class to create classes with a category. I'm only used by Tests.

I am similar to ClassFactoryForTestCase (i.e., I can be used in place of a ClassFactoryForTestCase), expect that classes may be created on a specific class category.
"
Class {
	#name : #ClassFactoryWithOrganization,
	#superclass : #ClassFactoryForTestCase,
	#instVars : [
		'organization'
	],
	#category : #'SUnit-Core-Extensions'
}

{ #category : #'instance creation' }
ClassFactoryWithOrganization class >> newWithOrganization: aSystemOrganizer [
	^self new
		organization: aSystemOrganizer;
		yourself
]

{ #category : #cleaning }
ClassFactoryWithOrganization >> deletePackage [
	| categoriesMatchString |
	categoriesMatchString := self packageName, '-*'.
	self organization removeCategoriesMatching: categoriesMatchString ; 
		removeEmptyCategories
	
]

{ #category : #creating }
ClassFactoryWithOrganization >> newClassNamed: aString subclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString [ 
	^self 
		newClassNamed: aString
		subclassOf: aClass 
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		category:  (self packageName, '-', self defaultTagPostfix) asSymbol.
]

{ #category : #creating }
ClassFactoryWithOrganization >> newClassNamed: aString subclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [
	| newClass |
	newClass := aClass classInstaller
		make: [ :builder | 
			builder
				superclass: aClass;
				name: aString;
				layoutClass: aClass classLayout class;
				slots: ivNamesString asSlotCollection;
				sharedVariablesFromString: classVarsString;
				sharedPools: '';
				category: category asSymbol;
				environment: self organization environment ].
	self createdClasses add: newClass.
	^ newClass
]

{ #category : #creating }
ClassFactoryWithOrganization >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames:  classVarsString [ 
	^self 
		newSubclassOf: aClass 
		instanceVariableNames: ivNamesString 
		classVariableNames: classVarsString 
		category:  (self packageName, '-', self defaultTagPostfix) asSymbol.
]

{ #category : #creating }
ClassFactoryWithOrganization >> newSubclassOf: aClass instanceVariableNames: ivNamesString classVariableNames: classVarsString category: category [
	| newClass |
	newClass := aClass classInstaller
		make: [ :builder | 
			builder
				superclass: aClass;
				name: self newClassName;
				layoutClass: aClass classLayout class;
				slots: ivNamesString asSlotCollection;
				sharedVariablesFromString: classVarsString;
				sharedPools: '';
				category: category asSymbol;
				environment: self organization environment ].
	self createdClasses add: newClass.
	^ newClass
]

{ #category : #accessing }
ClassFactoryWithOrganization >> organization [
	^organization
]

{ #category : #accessing }
ClassFactoryWithOrganization >> organization: aSystemOrganizer [
	organization := aSystemOrganizer
]
