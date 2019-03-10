"
An abstract announcement class for tool registry announcements
"
Class {
	#name : #ToolRegistryAnnouncement,
	#superclass : #Announcement,
	#instVars : [
		'tool',
		'name'
	],
	#category : #'Tool-Registry'
}

{ #category : #'instance creation' }
ToolRegistryAnnouncement class >> tool: aTool name: aSymbol [

	^ self new 
		tool: aTool;
		name: aSymbol;
		yourself
]

{ #category : #accessing }
ToolRegistryAnnouncement >> name [
	^ name
]

{ #category : #accessing }
ToolRegistryAnnouncement >> name: anObject [
	name := anObject
]

{ #category : #accessing }
ToolRegistryAnnouncement >> tool [
	^ tool
]

{ #category : #accessing }
ToolRegistryAnnouncement >> tool: anObject [
	tool := anObject
]
