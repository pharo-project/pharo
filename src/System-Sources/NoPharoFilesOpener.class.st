"
I am NoPharoFilesOpener.

I am a PharoFilesOpener that does nothing. I.e. neither the sources nor the changes file are opened and nil is returned.

I am used to set up an image to run an image without sources or changes.

To install me, execute

  NoPharoFilesOpener install.

To get back to the normal situation, execute

  PharoFilesOpener reset.
"
Class {
	#name : #NoPharoFilesOpener,
	#superclass : #PharoFilesOpener,
	#category : #'System-Sources-Utilities'
}

{ #category : #singleton }
NoPharoFilesOpener class >> install [
	Default := self new
]

{ #category : #public }
NoPharoFilesOpener >> changesFileOrNil [
	^ nil
]

{ #category : #public }
NoPharoFilesOpener >> sourcesFileOrNil [
	^ nil
]
