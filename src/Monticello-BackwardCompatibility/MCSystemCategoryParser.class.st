"
A MCSystemCategoryParser extracts or set a category/protocol to the corresponding MCOrganizationDefinition with the old definition
"
Class {
	#name : 'MCSystemCategoryParser',
	#superclass : 'MCDoItParser',
	#category : 'Monticello-BackwardCompatibility',
	#package : 'Monticello-BackwardCompatibility'
}

{ #category : 'factory identification hook' }
MCSystemCategoryParser class >> pattern [
	^ 'SystemOrganization*'
]

{ #category : 'actions' }
MCSystemCategoryParser >> addDefinitionsTo: aCollection [

	| definition category |
	definition := aCollection
		              detect: [ :ea | ea isOrganizationDefinition ]
		              ifNone: [ aCollection add: MCOrganizationDefinition new ].

	category := self category.

	"Let's find the package and the tag from the category."
	definition packageName ifNil: [
		definition packageName: ((self packageOrganizer packageMatchingExtensionName: category)
				 ifNil: [ category ]
				 ifNotNil: [ :package | package name ]) ].

	category = definition packageName ifFalse: [
		definition tagNames: (definition tagNames copyWith: ((category beginsWith: definition packageName , '-')
					  ifTrue: [ category withoutPrefix: definition packageName , '-' ]
					  ifFalse: [ category ])) ]
]

{ #category : 'accessing' }
MCSystemCategoryParser >> category [

	| tokens |
	tokens := source parseLiterals.
	tokens size = 3 ifFalse: [ self error: 'Unrecognized category definition' ].
	^ tokens at: 3
]
