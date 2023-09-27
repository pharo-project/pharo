"
I am a mock attribute to help the testing of the styler.
"
Class {
	#name : 'SHRBTextFlagAttribute',
	#superclass : 'TextAttribute',
	#instVars : [
		'name'
	],
	#category : 'Shout-Tests-Styling',
	#package : 'Shout-Tests',
	#tag : 'Styling'
}

{ #category : 'instance creation' }
SHRBTextFlagAttribute class >> named: aString [

	^ self new
		  name: aString;
		  yourself
]

{ #category : 'accessing' }
SHRBTextFlagAttribute >> name [

	^ name
]

{ #category : 'accessing' }
SHRBTextFlagAttribute >> name: anObject [

	name := anObject
]

{ #category : 'printing' }
SHRBTextFlagAttribute >> printOn: aStream [

	super printOn: aStream.

	aStream
		nextPutAll: ' (';
		nextPutAll: name;
		nextPut: $)
]
