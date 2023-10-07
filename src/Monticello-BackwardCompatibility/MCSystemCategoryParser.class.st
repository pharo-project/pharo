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

	| definition |
	definition := aCollection
		              detect: [ :ea | ea isOrganizationDefinition ]
		              ifNone: [ aCollection add: MCOrganizationDefinition new ].
	"The next part is a hack because the backup monticello format is using a mix of packages and tags as one string for the organization. We are trying to guess what is the package and what is the tag even if the way to do so is not nice and is brittle. But we should not end up here anyway."
	definition packageName
		ifNil: [ definition packageName: self category ]
		ifNotNil: [
			| category |
			category := self category.
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
