Class {
	#name : #MCSerializationTest,
	#superclass : #MCTestCase,
	#category : #'Monticello-Tests'
}

{ #category : #asserting }
MCSerializationTest >> assertDependenciesMatchWith: writerClass [
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersionWithDependencies.
	stream := RWBinaryOrTextStream on: String new.
	writerClass fileOut: expected on: stream.
	actual := (readerClass on: stream reset) dependencies.
	self assert: actual = expected dependencies.
]

{ #category : #asserting }
MCSerializationTest >> assertExtensionProvidedBy: aClass [
	aClass readerClass extension
]

{ #category : #asserting }
MCSerializationTest >> assertSnapshotsMatchWith: writerClass [
	| readerClass expected stream actual |
	readerClass := writerClass readerClass.
	expected := self mockSnapshot.
	stream := RWBinaryOrTextStream on: String new.
	(writerClass on: stream) writeSnapshot: expected.
	actual := readerClass snapshotFromStream: stream reset.
	self assertSnapshot: actual matches: expected.
]

{ #category : #asserting }
MCSerializationTest >> assertVersionInfosMatchWith: writerClass [
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersion.
	stream := RWBinaryOrTextStream on: String new.
	writerClass fileOut: expected on: stream.
	actual := readerClass versionInfoFromStream: stream reset.
	self assert: actual = expected info.
]

{ #category : #asserting }
MCSerializationTest >> assertVersionsMatchWith: writerClass [
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersion.
	stream := RWBinaryOrTextStream on: String new.
	writerClass fileOut: expected on: stream.
	actual := readerClass versionFromStream: stream reset.
	self assertVersion: actual matches: expected.
]

{ #category : #mocks }
MCSerializationTest >> mockDiffyVersion [
	| repos workingCopy base next |
	repos := MCDictionaryRepository new.
	workingCopy := MCWorkingCopy forPackage: self mockPackage.
	workingCopy repositoryGroup addRepository: repos.
	MCRepositoryGroup default removeRepository: repos.
	base := self mockVersion.
	repos storeVersion: base.
	self change: #a toReturn: 'a2'.
	next := self mockVersionWithAncestor: base.
	^ next asDiffAgainst: base	
]

{ #category : #testing }
MCSerializationTest >> testMcdSerialization [
	| stream expected actual |
	expected := self mockDiffyVersion.
	stream := RWBinaryOrTextStream on: String new.
	MCMcdWriter fileOut: expected on: stream.
	actual := MCMcdReader versionFromStream: stream reset.
	self assertVersion: actual matches: expected.
]

{ #category : #testing }
MCSerializationTest >> testMczSerialization [
	self assertVersionsMatchWith: MCMczWriter.
	self assertExtensionProvidedBy: MCMczWriter.
	self assertVersionInfosMatchWith: MCMczWriter.
	self assertDependenciesMatchWith: MCMczWriter.
]

{ #category : #testing }
MCSerializationTest >> testStSerialization [
	self assertSnapshotsMatchWith: MCStWriter.
]
