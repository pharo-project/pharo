"
I return all classes without any tag
"
Class {
	#name : 'ClyUntaggedClassesQuery',
	#superclass : 'ClyClassQuery',
	#category : 'Calypso-SystemQueries-Queries',
	#package : 'Calypso-SystemQueries',
	#tag : 'Queries'
}

{ #category : 'printing' }
ClyUntaggedClassesQuery >> description [

	^'classes without tags'
]

{ #category : 'testing' }
ClyUntaggedClassesQuery >> selectsClass: aClass [
	"The real fix here is to reify package tags in Ring (which is planned to be removed).
	We ask the realClass so we can send packageTag without getting an error.
	See issue #15931 for details"

	^ aClass isRGObject
		  ifTrue: [ aClass packageTag isNotNil ]
		  ifFalse: [ aClass packageTag isRoot ]
]
