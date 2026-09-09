"
I am NoChangesLog.

I am a ChangesLog that does nothing. I.e. I do not log any changes.

I am used to set up an image to run an image without changes.

To install me, execute

  NoChangesLog install.

To get back to the normal situation, execute

  ChangesLog reset.
"
Class {
	#name : 'NoChangesLog',
	#superclass : 'ChangesLog',
	#category : 'Deprecated14-Kernel',
	#package : 'Deprecated14-Kernel'
}

{ #category : 'singleton' }
NoChangesLog class >> install [

	self flag: #todoSource. "Add the deprecation message"
	self deprecated: ''
]
