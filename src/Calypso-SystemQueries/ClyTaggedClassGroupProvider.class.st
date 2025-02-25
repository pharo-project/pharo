"
I provide tagged class groups. 

I collect all class tags from the package scope and create ClyTaggedClassGroup instances for each of them
"
Class {
	#name : 'ClyTaggedClassGroupProvider',
	#superclass : 'ClyPackageTagProvider',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'building groups' }
ClyTaggedClassGroupProvider >> packageTagssIn: aPackageScope do: aBlock [

	| classGroups group |
	classGroups := Dictionary new.
	aPackageScope packagesDo: [ :package |
		package tagsForClasses do: [ :tag |
			classGroups at: tag ifAbsentPut: [
				group := ClyTaggedClassGroup withClassesFrom: aPackageScope taggedBy: tag.
				aBlock value: group.
				group ] ] ]
]
