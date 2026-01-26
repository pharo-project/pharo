"
I represent the changes file and give access to him.  If you want to log a change, look at my #logChange: method.

I have a default instance that is hearing SystemAnnouncements and logging the change logs in there.
But you can have your own instance and log whatever you want.
"
Class {
	#name : 'ChangesLog',
	#superclass : 'Object',
	#category : 'Deprecated14-Kernel',
	#package : 'Deprecated14-Kernel'
}

{ #category : 'accessing' }
ChangesLog class >> default [

	^ SourceFileArray default
]

{ #category : 'testing' }
ChangesLog class >> isDeprecated [

	^ true
]
