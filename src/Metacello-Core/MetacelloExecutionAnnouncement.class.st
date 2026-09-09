"
I am an abstract class for metacello announcements related to the execution of Metacello.

## Instance Variables

| Attribute | Type | Description |
|---|---|---|
| executor | aMetacelloExecutor | The instance of the script executor linked to the announcement. |
"
Class {
	#name : 'MetacelloExecutionAnnouncement',
	#superclass : 'Announcement',
	#instVars : [
		'executor'
	],
	#category : 'Metacello-Core-Announcements',
	#package : 'Metacello-Core',
	#tag : 'Announcements'
}

{ #category : 'instance creation' }
MetacelloExecutionAnnouncement class >> executor: anExecutor [

	^ self new
		  executor: anExecutor;
		  yourself
]

{ #category : 'testing' }
MetacelloExecutionAnnouncement class >> isAbstract [

	^ self = MetacelloExecutionAnnouncement
]

{ #category : 'accessing' }
MetacelloExecutionAnnouncement >> executor [

	^ executor
]

{ #category : 'accessing' }
MetacelloExecutionAnnouncement >> executor: anObject [

	executor := anObject
]
