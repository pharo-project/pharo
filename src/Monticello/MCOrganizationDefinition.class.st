"
A MCOrganizationDefinition represents a category change. 
"
Class {
	#name : #MCOrganizationDefinition,
	#superclass : #MCDefinition,
	#instVars : [
		'categories',
		'packageName',
		'tagNames'
	],
	#category : #'Monticello-Modeling',
	#package : #Monticello,
	#tag : #Modeling
}

{ #category : #'instance creation' }
MCOrganizationDefinition class >> categories: aCollection [
	^ self new categories: aCollection
]

{ #category : #'instance creation' }
MCOrganizationDefinition class >> packageName: aPackageName [

	^ self new
		  packageName: aPackageName asSymbol;
		  yourself
]

{ #category : #'instance creation' }
MCOrganizationDefinition class >> packageName: aPackageName tagNames: aCollection [

	^ (self packageName: aPackageName)
		  tagNames: aCollection;
		  yourself
]

{ #category : #comparing }
MCOrganizationDefinition >> = aDefinition [

	^ (super = aDefinition)
		and: [ self categories size = aDefinition categories size and: [ self categories includesAll: aDefinition categories ] ].
]

{ #category : #accessing }
MCOrganizationDefinition >> accept: aVisitor [
	^ aVisitor visitOrganizationDefinition: self
]

{ #category : #private }
MCOrganizationDefinition >> basicCommonPrefix [
	"Answers the minimum common denominator on package names contained in the monticello package. 
	 It can answer a package in the form X-Y-, with a minus at end..."
	| stream  |
	self categories ifEmpty: [ ^ '' ].
	
	stream := String new writeStream.
	self categories first withIndexDo: [:c :index |
		self categories do: [:each |
			(each at: index ifAbsent: []) = c 
				ifFalse: [ ^ stream contents ] ].
		stream nextPut: c ].
	
	^stream contents
]

{ #category : #accessing }
MCOrganizationDefinition >> categories [

	^ categories ifNil: [
		  self packageName
			  ifNil: [ {  } ]
			  ifNotNil: [ :package |
				  { package } , (self tagNames
					   reject: [ :tagName | tagName = package ]
					   thenCollect: [ :tagName | package , '-' , tagName ]) ] ]
]

{ #category : #deprecated }
MCOrganizationDefinition >> categories: aCollection [
	"Do not use. This methods should be replaced by #packageName and #tagNames"

	categories := aCollection sorted asArray
]

{ #category : #accessing }
MCOrganizationDefinition >> commonPrefix [
	"Answer the package common name. 
	 In ancient times, this was answering X-Y-, with a minus at the end... this was useful for the 
	 old PackageInfo framework. RPackage does not do pattern matching, thus this is not needed anymore 
	 (and provoques an error)"
	| prefix |
	prefix := self basicCommonPrefix.
	^(prefix notEmpty and: [ prefix endsWith: '-' ])
		ifTrue: [ prefix allButLast ]
		ifFalse: [ prefix ].
	

]

{ #category : #accessing }
MCOrganizationDefinition >> description [
	^ Array with: #organization with: self fullDescription
]

{ #category : #private }
MCOrganizationDefinition >> fullDescription [
	self categories ifEmpty: [ ^ self commonPrefix ].
	^ String
		streamContents: [ :stream | 
			| first |
			first := self categories first.
			stream << first.
			self categories allButFirst
				do: [ :c | 
					| part |
					part := c withoutPrefix: first.
					stream << $| << part ] ]
]

{ #category : #comparing }
MCOrganizationDefinition >> hash [
	^ (self species hash 
		bitXor: super hash)
		bitXor: self categories hash
]

{ #category : #initialization }
MCOrganizationDefinition >> initialize [
	super initialize.
	tagNames := #()
]

{ #category : #testing }
MCOrganizationDefinition >> isOrganizationDefinition [
	^ true
]

{ #category : #accessing }
MCOrganizationDefinition >> packageName [

	^ packageName
]

{ #category : #accessing }
MCOrganizationDefinition >> packageName: anObject [

	packageName := anObject
]

{ #category : #installing }
MCOrganizationDefinition >> postloadOver: oldDefinition [
	"Nothing to do"
]

{ #category : #accessing }
MCOrganizationDefinition >> shortSummaryPrefix [

	^ self definition description last
]

{ #category : #accessing }
MCOrganizationDefinition >> sortKey [
	^ '<organization>'
]

{ #category : #accessing }
MCOrganizationDefinition >> source [

	^ String streamContents: [ :s | self categories do: [ :ea | s nextPutAll: ea ] separatedBy: [ s cr ] ]
]

{ #category : #accessing }
MCOrganizationDefinition >> summary [

	^ self categories asArray printString
]

{ #category : #accessing }
MCOrganizationDefinition >> tagNames [

	^ tagNames
]

{ #category : #accessing }
MCOrganizationDefinition >> tagNames: aCollection [
	"ensure the tags are sorted alphabetically, so the merge don't take it as a conflict"

	tagNames := aCollection sorted asArray
]

{ #category : #unloading }
MCOrganizationDefinition >> unload [

	self categories do: [ :category | (self packageOrganizer isEmptyCategoryNamed: category) ifTrue: [ self packageOrganizer removeCategory: category ] ]
]
