Class {
	#name : 'MetacelloSpecTestCase',
	#superclass : 'TestCase',
	#category : 'Metacello-TestsCore-Specs',
	#package : 'Metacello-TestsCore',
	#tag : 'Specs'
}

{ #category : 'specs' }
MetacelloSpecTestCase >> groupSpec [
    ^ self project groupSpec
]

{ #category : 'accessing' }
MetacelloSpecTestCase >> packageSpec [

	^ self project packageSpec
]

{ #category : 'specs' }
MetacelloSpecTestCase >> packagesSpec [
    ^ self project packagesSpec
]

{ #category : 'accessing' }
MetacelloSpecTestCase >> project [
    ^ self projectWith: #()
]

{ #category : 'accessing' }
MetacelloSpecTestCase >> projectClass [
    ^ MetacelloProject
]

{ #category : 'specs' }
MetacelloSpecTestCase >> projectReferenceSpec [
    ^ self project projectReferenceSpec
]

{ #category : 'specs' }
MetacelloSpecTestCase >> projectSpec [
    ^ self project projectSpec
]

{ #category : 'accessing' }
MetacelloSpecTestCase >> projectWith: projectAttributes [

	| project |
	"Construct Metacello project"
	project := self projectClass new.
	project projectAttributes: projectAttributes.
	MetacelloVersionConstructor on: self project: project.
	^ project
]

{ #category : 'accessing' }
MetacelloSpecTestCase >> repositoriesSpec [

	^ self project repositoriesSpec
]

{ #category : 'accessing' }
MetacelloSpecTestCase >> repositorySpec [

	^ self project repositorySpec
]

{ #category : 'specs' }
MetacelloSpecTestCase >> valueHolderSpec [
    ^ self project valueHolderSpec
]

{ #category : 'specs' }
MetacelloSpecTestCase >> versionSpec [
    ^ self project versionSpec
]
