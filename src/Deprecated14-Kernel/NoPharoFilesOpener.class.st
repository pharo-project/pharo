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
	#name : 'NoPharoFilesOpener',
	#superclass : 'PharoFilesOpener',
	#category : 'Deprecated14-Kernel',
	#package : 'Deprecated14-Kernel'
}

{ #category : 'singleton' }
NoPharoFilesOpener class >> install [

	NoSourcesManager beCurrent
]

{ #category : 'testing' }
NoPharoFilesOpener class >> isDeprecated [
	"Use `NoSourcesManager beCurrent instead."

	^ true
]

{ #category : 'public' }
NoPharoFilesOpener >> changesFileOrNil [
	^ nil
]

{ #category : 'public' }
NoPharoFilesOpener >> sourcesFileOrNil [
	^ nil
]
