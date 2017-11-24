"
I am an annoucement use by Glamour to signal that the dataSource of a Renderer changed.

Description 
--------------------

I keep a pointer to the new dataSource. 
I can be use when the user filter a FastTable for example.
I am use by a Fast related renderer in GLMMorphicFTRenderer.

Public API and Key Messages
------------------

- newDataSource: aDataSource 		is my constructor.
		
Internal Representation and Key Implementation Points.
------------------ 

    Instance Variables
	newDataSource:		The new DataSource.

"
Class {
	#name : #GLMDataSourceUpdated,
	#superclass : #Announcement,
	#instVars : [
		'newDataSource'
	],
	#category : #'Glamour-FastTable-Announcement'
}

{ #category : #'instance creation' }
GLMDataSourceUpdated class >> newDataSource: aDataSource [
	^ self new
		newDataSource: aDataSource;
		yourself
]

{ #category : #accessing }
GLMDataSourceUpdated >> newDataSource [
	^ newDataSource
]

{ #category : #accessing }
GLMDataSourceUpdated >> newDataSource: anObject [
	newDataSource := anObject
]
