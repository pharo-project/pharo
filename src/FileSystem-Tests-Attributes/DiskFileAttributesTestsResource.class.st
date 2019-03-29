"
DiskFileAttributesTestsResources provides a file with known attributes that can be used to test attribute retrieval in the File System.

DiskFileAttributesTestsResources is currently used by FileReferenceAttributeTests.

    Instance Variables
	afterCreationTime:	The time just after file was created	<DateAndTime>
	beforeCreationTime:	The time just before file was created	<DateAndTime>
	file:					The name of the file (with known attributes)	<FileReference>


    Implementation Points
"
Class {
	#name : #DiskFileAttributesTestsResource,
	#superclass : #TestResource,
	#instVars : [
		'file',
		'beforeCreationTime',
		'afterCreationTime'
	],
	#category : #'FileSystem-Tests-Attributes-Resources'
}

{ #category : #accessing }
DiskFileAttributesTestsResource >> afterCreationTime [
	^ afterCreationTime
]

{ #category : #accessing }
DiskFileAttributesTestsResource >> beforeCreationTime [
	^ beforeCreationTime
]

{ #category : #accessing }
DiskFileAttributesTestsResource >> file [
	^ file
]

{ #category : #running }
DiskFileAttributesTestsResource >> setUp [

	| ws |

	super setUp.
	file := FileLocator temp / ('FileAttributesTests-žřč-', UUIDGenerator next asString, '.txt').
	beforeCreationTime := DateAndTime now.
	"Remove the nanoseconds"
	beforeCreationTime := DateAndTime
		date: beforeCreationTime asDate
		time: (Time seconds: beforeCreationTime secondsSinceMidnightLocalTime nanoSeconds: 0).
	ws := file writeStream.
	[
		ws nextPutAll: 'Created by FileAttributesTestsResources>>setUp '.
		beforeCreationTime printOn: ws.
	] ensure: [ ws close ].
	afterCreationTime := DateAndTime now rounded.
	"Round up to the next second"
	afterCreationTime := DateAndTime
		date: beforeCreationTime asDate
		time: (Time seconds: afterCreationTime secondsSinceMidnightLocalTime + 1 nanoSeconds: 0).

]

{ #category : #running }
DiskFileAttributesTestsResource >> tearDown [

	super tearDown.
	file delete.
]
