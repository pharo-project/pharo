"
I provide tagged class groups. 

I collect all class tags from the package scope and create ClyTaggedClassGroup instances for each of them
"
Class {
	#name : #ClyTaggedClassGroupProvider,
	#superclass : #ClyClassGroupProvider,
	#category : #'Calypso-SystemQueries-Domain'
}

{ #category : #'building groups' }
ClyTaggedClassGroupProvider >> classGroupsIn: aPackageScope do: aBlock [
	| classGroups group |
	classGroups := Dictionary new.	
	aPackageScope packagesDo: [ :each |
		each tagsForClasses do: [ :eachTag |	
			classGroups at: eachTag ifAbsentPut: [
				group := ClyTaggedClassGroup 
					withClassesFrom: aPackageScope taggedBy: eachTag.
				aBlock value: group.
				group].				
	]]
]
