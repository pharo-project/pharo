"
MCPackage represents a package. It is merely a wrapper on top of a package set or packageInfo.

Strangely enough it does not inherit from MCDefinition.

Its most important method is snapshot which returns a snapshot with all the entities that should be saved. 
"
Class {
	#name : 'MCPackage',
	#superclass : 'Object',
	#instVars : [
		'name',
		'environment'
	],
	#category : 'Monticello-Base',
	#package : 'Monticello',
	#tag : 'Base'
}

{ #category : 'instance creation' }
MCPackage class >> named: aString [
	^ self new name: aString
]

{ #category : 'comparing' }
MCPackage >> = other [
	^ other species = self species and: [other name = name]
]

{ #category : 'printing' }
MCPackage >> displayStringOn: aStream [
	aStream nextPutAll: self name
]

{ #category : 'accessing' }
MCPackage >> environment [
	"Allows to have MCPackage that are link to system packages from a non default package organizer."

	^ environment ifNil: [ self class environment ]
]

{ #category : 'accessing' }
MCPackage >> environment: anObject [

	environment := anObject
]

{ #category : 'working copies' }
MCPackage >> hasWorkingCopy [
	^ MCWorkingCopy registry includesKey: self
]

{ #category : 'comparing' }
MCPackage >> hash [
	^ name asLowercase hash
]

{ #category : 'testing' }
MCPackage >> isDirty [
	
	^ self workingCopy
		ifNil: [ false ]
		ifNotNil: [ :wc | wc modified ]
]

{ #category : 'accessing' }
MCPackage >> name [
	^ name
]

{ #category : 'accessing' }
MCPackage >> name: aString [
	name := aString
]

{ #category : 'printing' }
MCPackage >> printOn: aStream [
	super printOn: aStream.
	aStream
		nextPut: $(;
		nextPutAll: name;
		nextPut: $)
]

{ #category : 'accessing' }
MCPackage >> snapshot [

	^ MCSnapshot fromDefinitions: (self systemPackage
			   ifNil: [ #(  ) ]
			   ifNotNil: [ :systemPackage |
				   OrderedCollection new
					   add: systemPackage asMCOrganizationDefinition;
					   addAll: (systemPackage methods collect: [ :method | method asMCMethodDefinition ]);
					   addAll: (systemPackage definedClasses collect: [ :class | class asClassDefinition ]);
					   yourself ])
]

{ #category : 'printing' }
MCPackage >> storeOn: aStream [
	aStream
		nextPutAll: 'MCPackage';
		space; nextPutAll: 'named: '; store: name.
]

{ #category : 'accessing' }
MCPackage >> systemPackage [
	"Return the corresponding system package. Return nil if no package in the system has this name."

	^ self environment organization packageNamed: self name ifAbsent: [ nil ]
]

{ #category : 'working copies' }
MCPackage >> unload [
	^ self workingCopy unload
]

{ #category : 'working copies' }
MCPackage >> workingCopy [

	^ MCWorkingCopy ensureForPackage: self
]
