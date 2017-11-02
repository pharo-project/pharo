Class {
	#name : #MetacelloSpecTestCase,
	#superclass : #TestCase,
	#category : 'Metacello-TestsCore-Specs'
}

{ #category : #specs }
MetacelloSpecTestCase >> groupSpec [
    ^ self project groupSpec
]

{ #category : #specs }
MetacelloSpecTestCase >> packagesSpec [
    ^ self project packagesSpec
]

{ #category : #accessing }
MetacelloSpecTestCase >> project [
    ^ self projectWith: #()
]

{ #category : #accessing }
MetacelloSpecTestCase >> projectClass [
    ^ MetacelloProject
]

{ #category : #specs }
MetacelloSpecTestCase >> projectReferenceSpec [
    ^ self project projectReferenceSpec
]

{ #category : #specs }
MetacelloSpecTestCase >> projectSpec [
    ^ self project projectSpec
]

{ #category : #accessing }
MetacelloSpecTestCase >> projectWith: projectAttributes [
    | project |
    "Construct Metacello project"
    project := self projectClass new.
    project projectAttributes: projectAttributes.
    MetacelloVersionConstructor on: self project: project.
    project loader: MetacelloNullRecordingMCSpecLoader new.
    ^ project
]

{ #category : #specs }
MetacelloSpecTestCase >> valueHolderSpec [
    ^ self project valueHolderSpec
]

{ #category : #specs }
MetacelloSpecTestCase >> versionSpec [
    ^ self project versionSpec
]
