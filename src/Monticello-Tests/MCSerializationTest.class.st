Class {
	#name : 'MCSerializationTest',
	#superclass : 'MCTestCase',
	#category : 'Monticello-Tests-IO',
	#package : 'Monticello-Tests',
	#tag : 'IO'
}

{ #category : 'asserting' }
MCSerializationTest >> assertDependenciesMatchWith: writerClass [
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersionWithDependencies.
	stream := ReadWriteStream on: ByteArray new.
	writerClass fileOut: expected on: stream.
	actual := (readerClass on: stream reset) dependencies.
	self assert: actual equals: expected dependencies.
]

{ #category : 'asserting' }
MCSerializationTest >> assertExtensionProvidedBy: aClass [
	aClass readerClass extension
]

{ #category : 'asserting' }
MCSerializationTest >> assertVersionInfosMatchWith: writerClass [
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersion.
	stream := ReadWriteStream on: ByteArray new.
	writerClass fileOut: expected on: stream.
	actual := readerClass versionInfoFromStream: stream reset.
	self assert: actual equals: expected info.
]

{ #category : 'asserting' }
MCSerializationTest >> assertVersionsMatchWith: writerClass [
	| stream readerClass expected actual |
	readerClass := writerClass readerClass.
	expected := self mockVersion.
	stream := ReadWriteStream on: ByteArray new.
	writerClass fileOut: expected on: stream.
	actual := readerClass versionFromStream: stream reset.
	self assertVersion: actual matches: expected.
]

{ #category : 'testing' }
MCSerializationTest >> testMczSerialization [
	self assertVersionsMatchWith: MCMczWriter.
	self assertExtensionProvidedBy: MCMczWriter.
	self assertVersionInfosMatchWith: MCMczWriter.
	self assertDependenciesMatchWith: MCMczWriter.
]

{ #category : 'testing' }
MCSerializationTest >> testStSerialization [

	| readerClass expected stream actual |
	readerClass := MCStWriter readerClass.
	expected := self mockSnapshot.
	stream := ReadWriteStream on: ''.
	(MCStWriter on: stream) writeSnapshot: expected.
	actual := (readerClass on: stream reset) snapshot.
	self assertSnapshot: actual matches: expected
]
