"
MethodChangeRecords are used to record method changes. Here is a simple summary of the relationship between the changeType symbol and the recording of prior state

			|	prior == nil			|	prior not nil	
	---------	|----------------------------	|--------------------
	add		|	add					|	change
	---------	|----------------------------	|--------------------
	remove	|	addedThenRemoved	|	remove

Structure:
changeType			symbol -- as summarized above
currentMethod	method
				This is the current version of the method.
				It can be used to assert this change upon entry to a layer. 
lastSourcePointer -- The source index of the last version of the method.

Note that the above states each have an associated revoke action:
	add --> remove
	change --> change back
	remove --> add back
	addedThenRemoved --> no change
However all of these are accomplished trivially by restoring the original method dictionary.
"
Class {
	#name : 'MethodChangeRecord',
	#superclass : 'Object',
	#instVars : [
		'changeType',
		'lastSourcePointer'
	],
	#category : 'System-Changes-Records',
	#package : 'System-Changes',
	#tag : 'Records'
}

{ #category : 'accessing' }
MethodChangeRecord >> changeType [

	^ changeType
]

{ #category : 'accessing' }
MethodChangeRecord >> lastSourcePointer [

	^ lastSourcePointer
]

{ #category : 'accessing' }
MethodChangeRecord >> lastSourcePointer: sourcePointer [
	"Store the source index of the last version of the method so it can be put back there if re-accepted from a version browser."

	lastSourcePointer := sourcePointer
]

{ #category : 'all changes' }
MethodChangeRecord >> noteChangeType: newChangeType [
	changeType := (changeType == #addedThenRemoved and: [ newChangeType == #change ])
		ifTrue: [ #add ]
		ifFalse: [ newChangeType ]
]

{ #category : 'printing' }
MethodChangeRecord >> printOn: strm [

	super printOn: strm.
	strm
		nextPutAll: ' (';
		print: changeType;
		nextPutAll: ')'
]
