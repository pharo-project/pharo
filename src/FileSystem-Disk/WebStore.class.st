Class {
	#name : #WebStore,
	#superclass : #DiskStore,
	#category : #'FileSystem-Disk-Store'
}

{ #category : #public }
WebStore class >> delimiter [
	^$/
]

{ #category : #public }
WebStore class >> maxFileNameLength [
	^255
]

{ #category : #abstract }
WebStore >> basicIsWritable: aNode [
	"Used to decide whether the low-level representation (node / entry) from the store is 
	a writable file or can be changed."
	^true
]
