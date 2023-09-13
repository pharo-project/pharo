"
A PackageLoader doing some additional cross-package checks
"
Class {
	#name : #MCMultiPackageLoader,
	#superclass : #MCPackageLoader,
	#category : #'Monticello-Loading'
}

{ #category : #private }
MCMultiPackageLoader >> analyze [
	| index |
	index := MCDefinitionIndex definitions: additions.
	removals removeAllSuchThat: [:removal |
		(index definitionLike: removal
			ifPresent: [:addition | obsoletions at: addition put: removal]
			ifAbsent: []) notNil].
	super analyze
]
