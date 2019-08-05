"
A MCOrganizationDefinition represents a category change. 
"
Class {
	#name : #MCOrganizationDefinition,
	#superclass : #MCDefinition,
	#instVars : [
		'categories'
	],
	#category : #'Monticello-Modeling'
}

{ #category : #'instance creation' }
MCOrganizationDefinition class >> categories: anArray [
	^ self new categories: anArray
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
	categories isEmpty ifTrue: [ ^ '' ].
	
	stream := String new writeStream.
	categories first withIndexDo: [:c :index |
		categories do: [:each |
			(each at: index ifAbsent: []) = c 
				ifFalse: [ ^ stream contents ] ].
		stream nextPut: c ].
	
	^stream contents
]

{ #category : #accessing }
MCOrganizationDefinition >> categories [
	"ensure the categories are sorted alphabetically, so the merge don't take it as a conflict"
	^ categories
]

{ #category : #accessing }
MCOrganizationDefinition >> categories: anArray [
	categories := anArray sort
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

{ #category : #testing }
MCOrganizationDefinition >> isOrganizationDefinition [
	^ true
]

{ #category : #installing }
MCOrganizationDefinition >> postloadOver: oldDefinition [
	SystemOrganization categories:
		(self
			reorderCategories: SystemOrganization categories
			original: (oldDefinition ifNil: [#()] ifNotNil: [oldDefinition categories]))
]

{ #category : #private }
MCOrganizationDefinition >> reorderCategories: allCategories original: oldCategories [

	^ allCategories
		detect: [ :ea | categories includes: ea ]
		ifFound: [ :first | 
			((allCategories copyUpTo: first)
				copyWithoutAll: oldCategories , categories) , categories
				,
					((allCategories copyAfter: first)
						copyWithoutAll: oldCategories , categories) ]
		ifNone: [ allCategories ]
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
	^ String streamContents:
		[:s |
		categories do: [:ea | s nextPutAll: ea] separatedBy: [s cr]]
]

{ #category : #accessing }
MCOrganizationDefinition >> summary [
	^ categories asArray printString
]

{ #category : #unloading }
MCOrganizationDefinition >> unload [
	categories
		do: [ :category | 
			(SystemOrganization isEmptyCategoryNamed: category)
				ifTrue: [ SystemOrganization removeCategory: category ] ]
]
