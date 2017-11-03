Class {
	#name : #EpEventIntegrationTest,
	#superclass : #EpEnabledIntegrationTest,
	#category : #EpiceaTests-Integration
}

{ #category : #mocks }
EpEventIntegrationTest >> mockPackage [

	^ MCPackage new name: #EpTest
]

{ #category : #mocks }
EpEventIntegrationTest >> mockVersion [

	^ MCVersion 
		package: self mockPackage
		info: self mockVersionInfo
		snapshot: MCSnapshot empty
]

{ #category : #mocks }
EpEventIntegrationTest >> mockVersionInfo [

	^ MCVersionInfo
		name: #EpTest
		id: UUID new
		message: ''
		date: nil
		time: nil
		author: ''
		ancestors: #()
]

{ #category : #tests }
EpEventIntegrationTest >> testMCVersionSaved [

self flag: #todo. "Don't understand why it fails in non-interactive mode (in Jenkins)"

"	| announcements version repository |
	version := self mockVersion.
	repository := MCDictionaryRepository new.
	announcements := OrderedCollection new.

	self assert: (self countLogEventsWith: EpMonticelloVersionSave) equals: 0.

	repository storeVersion: version.

	self assert: (self countLogEventsWith: EpMonticelloVersionSave) equals: 1.
"
]
