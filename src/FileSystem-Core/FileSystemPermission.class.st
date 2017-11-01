"
I'm a set of permissions for a Directory Entry
"
Class {
	#name : #FileSystemPermission,
	#superclass : #Object,
	#instVars : [
		'posixPermission'
	],
	#category : 'FileSystem-Core-Public'
}

{ #category : #'instance creation' }
FileSystemPermission class >> default [
	^ self posixPermissions: 8r777
]

{ #category : #'instance creation' }
FileSystemPermission class >> new [ 
	self error: 'Should not be called. Use #posixPermission: instead'
]

{ #category : #'instance creation' }
FileSystemPermission class >> posixPermissions: aNumber [
	^self basicNew 
		initialize: aNumber;
		yourself
]

{ #category : #comparing }
FileSystemPermission >> < other [
	^ posixPermission < other posixPermission
]

{ #category : #comparing }
FileSystemPermission >> <= other [
	^ (posixPermission > other posixPermission) not
]

{ #category : #comparing }
FileSystemPermission >> > other [
	^ other posixPermission < posixPermission
]

{ #category : #comparing }
FileSystemPermission >> >= other [
	^ other posixPermission <= posixPermission
]

{ #category : #accessing }
FileSystemPermission >> groupExecute [
	^ self permissionBitAt: 4
]

{ #category : #accessing }
FileSystemPermission >> groupRead [
	^ self permissionBitAt: 6
]

{ #category : #accessing }
FileSystemPermission >> groupWrite [
	^ self permissionBitAt: 5
]

{ #category : #initialization }
FileSystemPermission >> initialize: aNumber [
	posixPermission := aNumber.
	self initialize.	
]

{ #category : #testing }
FileSystemPermission >> isReadable [
	^ self ownerRead
]

{ #category : #testing }
FileSystemPermission >> isWritable [
	^ self ownerWrite
]

{ #category : #accessing }
FileSystemPermission >> otherExecute [
	^ self permissionBitAt: 1
]

{ #category : #accessing }
FileSystemPermission >> otherRead [
	^ self permissionBitAt: 3
]

{ #category : #accessing }
FileSystemPermission >> otherWrite [
	^ self permissionBitAt: 2
]

{ #category : #accessing }
FileSystemPermission >> ownerExecute [
	^ self permissionBitAt: 7
]

{ #category : #accessing }
FileSystemPermission >> ownerRead [
	^ self permissionBitAt: 9
]

{ #category : #accessing }
FileSystemPermission >> ownerWrite [
	^ self permissionBitAt: 8
]

{ #category : #accessing }
FileSystemPermission >> permissionBitAt: bitIndex [
	^ (posixPermission bitAt: bitIndex) == 1
]

{ #category : #private }
FileSystemPermission >> posixPermission [
	^ posixPermission
]

{ #category : #printing }
FileSystemPermission >> printOn: aStream [
	aStream 
		"Owner"
		nextPut: (self ownerRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self ownerWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self ownerExecute ifTrue: [ $x ] ifFalse: [ $- ]);
		"Group"
		nextPut: (self groupRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self groupWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self groupExecute ifTrue: [ $x ] ifFalse: [ $- ]);
		"Other"
		nextPut: (self otherRead ifTrue: [ $r ] ifFalse: [ $- ]);
		nextPut: (self otherWrite ifTrue: [ $w ] ifFalse: [ $- ]);
		nextPut: (self otherExecute ifTrue: [ $x ] ifFalse: [ $- ])
		
]
