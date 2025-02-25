"
A MCOrganizationDefinition represents a category change. 
"
Class {
	#name : 'MCOrganizationDefinition',
	#superclass : 'MCDefinition',
	#instVars : [
		'packageName',
		'tagNames'
	],
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'instance creation' }
MCOrganizationDefinition class >> packageName: aPackageName [

	^ self new
		  packageName: aPackageName asSymbol;
		  yourself
]

{ #category : 'instance creation' }
MCOrganizationDefinition class >> packageName: aPackageName tagNames: aCollection [

	^ (self packageName: aPackageName)
		  tagNames: aCollection;
		  yourself
]

{ #category : 'comparing' }
MCOrganizationDefinition >> = aDefinition [

	^ super = aDefinition and: [ packageName = aDefinition packageName and: [ tagNames = aDefinition tagNames ] ]
]

{ #category : 'accessing' }
MCOrganizationDefinition >> accept: aVisitor [
	^ aVisitor visitOrganizationDefinition: self
]

{ #category : 'accessing' }
MCOrganizationDefinition >> categories [

	^ self packageName
		  ifNil: [ {  } ]
		  ifNotNil: [ :package | { package } , (self tagNames collect: [ :tagName | package , '-' , tagName ]) ]
]

{ #category : 'accessing' }
MCOrganizationDefinition >> description [
	^ Array with: #organization with: self fullDescription
]

{ #category : 'private' }
MCOrganizationDefinition >> fullDescription [

	self tagNames ifNil: [ ^ self packageName ].

	^ String streamContents: [ :stream |
		  | first |
		  first := self categories first.
		  stream << first.
		  self categories allButFirst do: [ :c |
			  | part |
			  part := c withoutPrefix: first.
			  stream << $| << part ] ]
]

{ #category : 'comparing' }
MCOrganizationDefinition >> hash [

	^ ((self species hash bitXor: super hash) bitXor: self packageName hash) bitXor: self tagNames hash
]

{ #category : 'initialization' }
MCOrganizationDefinition >> initialize [
	super initialize.
	tagNames := #()
]

{ #category : 'testing' }
MCOrganizationDefinition >> isOrganizationDefinition [
	^ true
]

{ #category : 'accessing' }
MCOrganizationDefinition >> packageName [

	^ packageName
]

{ #category : 'accessing' }
MCOrganizationDefinition >> packageName: anObject [

	packageName := anObject
]

{ #category : 'installing' }
MCOrganizationDefinition >> postloadOver: oldDefinition [
	"Nothing to do"
]

{ #category : 'accessing' }
MCOrganizationDefinition >> sortKey [
	^ '<organization>'
]

{ #category : 'accessing' }
MCOrganizationDefinition >> source [

	^ String streamContents: [ :s | self categories do: [ :ea | s nextPutAll: ea ] separatedBy: [ s cr ] ]
]

{ #category : 'accessing' }
MCOrganizationDefinition >> summary [

	^ self categories asArray printString
]

{ #category : 'accessing' }
MCOrganizationDefinition >> tagNames [

	^ tagNames
]

{ #category : 'accessing' }
MCOrganizationDefinition >> tagNames: aCollection [
	"ensure the tags are sorted alphabetically, so the merge don't take it as a conflict"

	| tags |
	tags := aCollection.
	tags := tags \ { Package rootTagName }.
	tagNames := tags sorted asArray
]

{ #category : 'unloading' }
MCOrganizationDefinition >> unload [

	| package |
	package := self packageOrganizer packageNamed: self packageName ifAbsent: [ ^ self ].
	package removeEmptyTags.
	package isEmpty ifTrue: [ package removeFromSystem ]
]
