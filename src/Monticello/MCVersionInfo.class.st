"
Adds to the record of ancestry, other identifying details.
"
Class {
	#name : 'MCVersionInfo',
	#superclass : 'MCAncestry',
	#instVars : [
		'id',
		'name',
		'message',
		'date',
		'time'
	],
	#category : 'Monticello-Versioning',
	#package : 'Monticello',
	#tag : 'Versioning'
}

{ #category : 'instance creation' }
MCVersionInfo class >> name: vName id: id message: message date: date time: time ancestors: ancestors [
	^ self 
		name: vName
		id: id
		message: message
		date: date
		time: time
		ancestors: ancestors
		stepChildren: #()
]

{ #category : 'instance creation' }
MCVersionInfo class >> name: vName id: id message: message date: date time: time ancestors: ancestors stepChildren: stepChildren [
	^ self new
		initializeWithName: vName
		id: id
		message: message
		date: date
		time: time
		ancestors: ancestors
		stepChildren: stepChildren
]

{ #category : 'instance creation' }
MCVersionInfo class >> name: vName id: id message: message date: date time: time author: author ancestors: ancestors stepChildren: stepChildren [

	^ self
		  name: vName
		  id: id
		  message: message
		  date: date
		  time: time
		  ancestors: ancestors
		  stepChildren: stepChildren
]

{ #category : 'comparing' }
MCVersionInfo >> = other [
	^ other species = self species
		and: [other hasID: id]
]

{ #category : 'converting' }
MCVersionInfo >> asDictionary [
	^ Dictionary new
		at: #name put: name;
		at: #id put: id asString;
		at: #message put: message;
		at: #date put: date;
		at: #time put: time;
		at: #ancestors put: (self ancestors collect: [:a | a asDictionary]);
		yourself
]

{ #category : 'accessing' }
MCVersionInfo >> author [

	^ 'CompatibleUserName'
]

{ #category : 'pillaging' }
MCVersionInfo >> date [
	^ date
]

{ #category : 'displaying' }
MCVersionInfo >> displayStringOn: stream [
	stream nextPutAll: self name
]

{ #category : 'private' }
MCVersionInfo >> hasID: aUUID [
	^ id = aUUID
]

{ #category : 'comparing' }
MCVersionInfo >> hash [
	^ id hash
]

{ #category : 'pillaging' }
MCVersionInfo >> id [
	^ id 
]

{ #category : 'initialization' }
MCVersionInfo >> initializeWithName: vName id: aUUID message: aString date: aDate time: aTime ancestors: aCollection stepChildren: stepCollection [

	name := vName.
	id := aUUID.
	message := aString.
	date := aDate ifNil: [ Date today ].
	time := aTime.
	self setAncestors: aCollection.
	self setStepChildren: stepCollection
]

{ #category : 'initialization' }
MCVersionInfo >> lazy [
	^ MCLazyVersionInfo from: self
]

{ #category : 'private' }
MCVersionInfo >> loadAncestorsAndStepChildren [
	"This code scans all the repositories to find the exact package version and reload the relevant info."

	"Transient failures, such as the lack of network connectivity, will return nothing and no errors from this code."

	"Unused in MCVersionInfo, used only in MCLazyVersionInfo."

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"
	| r |
	r := MCRepositoryGroup default.
	r versionWithInfo: self ifNone: [ ].
	r
		ifNotNil:
			[ :aVersion | 
			self setAncestors: aVersion info ancestors.
			self setStepChildren: aVersion info stepChildren ]
]

{ #category : 'accessing' }
MCVersionInfo >> message [
	^ message ifNil: ['']
]

{ #category : 'accessing' }
MCVersionInfo >> name [
	^ name ifNil: ['<working copy>']
]

{ #category : 'printing' }
MCVersionInfo >> printOn: aStream [
	super printOn: aStream.
	aStream nextPut: $(; nextPutAll: self name; nextPut: $)
	
]

{ #category : 'private' }
MCVersionInfo >> setAncestors: someAncestors [
	"Unused in MCVersionInfo"

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"

	ancestors := someAncestors
]

{ #category : 'private' }
MCVersionInfo >> setStepChildren: someStepChildren [
	"Unused in MCVersionInfo"

	"It is there to allow a certain customisation with lazy stuff without kernel dependencies on LRUCache"

	stepChildren := someStepChildren
]

{ #category : 'pillaging' }
MCVersionInfo >> time [
	^ time
]

{ #category : 'accessing' }
MCVersionInfo >> timeStamp [
	^ DateAndTime date: date time: time
]

{ #category : 'accessing' }
MCVersionInfo >> timeString [
	^ date asString, ', ', time asString
]

{ #category : 'accessing' }
MCVersionInfo >> versionNumber [
	^ ((self name copyAfterLast: $-) copyAfter: $.) asInteger
]
