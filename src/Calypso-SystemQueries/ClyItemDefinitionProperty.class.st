"
I am a root of hierarchy of system object definitions.
System objects are supposed to have defining package and defining class.
And idea to decorate items with these information.

When system objects are converted to browser items they add system definition property: 
	
	Array asCalypsoBrowserItem 

And you can access definition from any item: 

	Array asCalypsoBrowserItem systemDefinition.

Definitions were introduced to cover remote scenario of browser where package and class should be loaded together with methods.
Query browser shows package and class for every item. So it is important to not request them from remote side for every item.
Maybe this approach will be replaced by better idea in future.

I provide suitavle instantiation method for my subclasses: 

- package: aPackage class: aClass 

Internally I convert given package and class to browser items. So my variables include wrapper.
To access actual class or package use: 
		itemDefinition definingClass
		itemDefinition definingPackage.

Notice that my defining class is always instance side of class. And I have extra flag isInstanceSide to indicate that object is actually defined on instance side or on class side. 
It can be also avoided in future because defining class can be metaclass when object is defined on class side.
But for now it was more suitable to use definitions in this form from the UI and in case of remote scenario.
   
Internal Representation and Key Implementation Points.

    Instance Variables
	definingClassItem:		<ClyBrowserItem>
	definingPackageItem:		<ClyBrowserItem>
	isInstanceSide:		<Boolean>
"
Class {
	#name : #ClyItemDefinitionProperty,
	#superclass : #ClyProperty,
	#instVars : [
		'definingClassItem',
		'definingPackageItem',
		'isInstanceSide'
	],
	#category : #'Calypso-SystemQueries-Properties'
}

{ #category : #'instance creation' }
ClyItemDefinitionProperty class >> package: aPackage class: aClass [
	
	^self new 
		definingClass: aClass instanceSide;
		isInstanceSide: aClass isInstanceSide;
		definingPackage: aPackage
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingClass [
	^self definingClassItem actualObject
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingClass: aClass [

	definingClassItem := ClyBrowserItem named: aClass name with: aClass
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingClassItem [
	^ definingClassItem
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingMetaLevelScope [
	^isInstanceSide 
		ifTrue: [ ClyInstanceSideScope ]
		ifFalse: [ ClyClassSideScope  ]
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingPackage [
	^self definingPackageItem actualObject
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingPackage: aPackage [

	definingPackageItem := ClyBrowserItem named: aPackage name with: aPackage
]

{ #category : #accessing }
ClyItemDefinitionProperty >> definingPackageItem [
	^ definingPackageItem
]

{ #category : #accessing }
ClyItemDefinitionProperty >> isInstanceSide [
	^ isInstanceSide
]

{ #category : #accessing }
ClyItemDefinitionProperty >> isInstanceSide: anObject [
	isInstanceSide := anObject
]

{ #category : #printing }
ClyItemDefinitionProperty >> printDefiningClass [

	^isInstanceSide 
		ifTrue: [ definingClassItem name]
		ifFalse: [ definingClassItem name, ' class']
]

{ #category : #'printing items' }
ClyItemDefinitionProperty >> printDefinition [

	self subclassResponsibility 
]

{ #category : #'printing items' }
ClyItemDefinitionProperty >> printFullNameOf: anEnvironmentItem [

	^anEnvironmentItem name, ' (', self printDefinition, ')'
]
