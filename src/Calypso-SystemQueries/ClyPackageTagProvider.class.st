"
I am a root of hierarchy of package tag providers.
My subclasses must build class groups for given package scope.

They should implement following method: 

- packageTagsIn: aPackageScope do: aBlock

It should create class group instances and pass them into the block.
Look at ClyClassGroup comment to see how create groups.
"
Class {
	#name : 'ClyPackageTagProvider',
	#superclass : 'Object',
	#category : 'Calypso-SystemQueries-Domain',
	#package : 'Calypso-SystemQueries',
	#tag : 'Domain'
}

{ #category : 'building groups' }
ClyPackageTagProvider >> packageTagssIn: aPackageScope do: aBlock [
	self subclassResponsibility
]

{ #category : 'system changes' }
ClyPackageTagProvider >> providesGroupsAffectedBy: aSystemAnnouncement inScope: aPackageScope [

	aPackageScope packagesDo: [ :each | (aSystemAnnouncement affectsPackage: each) ifTrue: [ ^ true ] ].
	^ false
]
