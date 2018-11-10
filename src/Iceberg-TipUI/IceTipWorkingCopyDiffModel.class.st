"
I'm a diff model that represents a diff between the HEAD commit and the image working copy.
"
Class {
	#name : #IceTipWorkingCopyDiffModel,
	#superclass : #IceTipDiffModel,
	#category : #'Iceberg-TipUI-Model'
}

{ #category : #accesing }
IceTipWorkingCopyDiffModel >> diff [
	^ diff ifNil: [ diff := self newDiff ]
]

{ #category : #testing }
IceTipWorkingCopyDiffModel >> isEmpty [
	^ self diff isEmpty
]

{ #category : #'private factory' }
IceTipWorkingCopyDiffModel >> newDiff [
	^ self entity workingCopyDiff
]

{ #category : #private }
IceTipWorkingCopyDiffModel >> resetDiff [
	diff := nil
]
