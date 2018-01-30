Class {
	#name : #SourceFile,
	#superclass : #Object,
	#instVars : [
		'path',
		'stream'
	],
	#category : #'System-Sources'
}

{ #category : #'instance creation' }
SourceFile class >> on: aPath [

	^ self new
		path: aPath;
		yourself
]

{ #category : #testing }
SourceFile >> atEnd [
	
	^ stream atEnd
]

{ #category : #'as yet unclassified' }
SourceFile >> close [

	stream close
]

{ #category : #accessing }
SourceFile >> closed [
	^ stream closed
]

{ #category : #'as yet unclassified' }
SourceFile >> cr [

	stream cr
]

{ #category : #'as yet unclassified' }
SourceFile >> flush [

	stream flush
]

{ #category : #'as yet unclassified' }
SourceFile >> fullName [

	^ path asString
]

{ #category : #'as yet unclassified' }
SourceFile >> isOpen [

	^ stream notNil and: [ stream closed not ]
]

{ #category : #'as yet unclassified' }
SourceFile >> isReadOnly [

	^ stream isReadOnly
]

{ #category : #'as yet unclassified' }
SourceFile >> next [

	^ stream next
]

{ #category : #'as yet unclassified' }
SourceFile >> next: anInteger [

	^ stream next: anInteger
]

{ #category : #'as yet unclassified' }
SourceFile >> nextPut: aCharacter [

	stream nextPut: aCharacter
]

{ #category : #'as yet unclassified' }
SourceFile >> nextPutAll: aString [

	stream nextPutAll: aString
]

{ #category : #accessing }
SourceFile >> path: arg1 [
	path := arg1
]

{ #category : #accessing }
SourceFile >> peek [
	
	^ stream peek
]

{ #category : #'as yet unclassified' }
SourceFile >> position [

	^ stream position
]

{ #category : #'as yet unclassified' }
SourceFile >> position: anInteger [

	stream position: anInteger
]

{ #category : #'as yet unclassified' }
SourceFile >> printOn: aStream [

	aStream
		nextPutAll: self class name;
		nextPut: $(;
		nextPutAll: self fullName;
		nextPut: $)
]

{ #category : #'as yet unclassified' }
SourceFile >> readOnlyCopy [

	^ (self species on: path)
		tryOpenReadOnly: true;
		yourself
]

{ #category : #'as yet unclassified' }
SourceFile >> setToEnd [

	stream setToEnd
]

{ #category : #'as yet unclassified' }
SourceFile >> size [

	^ stream size
]

{ #category : #'as yet unclassified' }
SourceFile >> skip: anInteger [

	stream skip: anInteger
]

{ #category : #'as yet unclassified' }
SourceFile >> space [

	stream space
]

{ #category : #'as yet unclassified' }
SourceFile >> tryOpen [

	self tryOpenReadOnly: false
]

{ #category : #'as yet unclassified' }
SourceFile >> tryOpenReadOnly: arg1 [
	| tmp1 tmp2 tmp3 |
	tmp1 := path asFileReference.
	tmp2 := FileLocator imageDirectory / tmp1 basename.
	tmp3 := Array with: tmp2 with: tmp1.
	arg1
		ifFalse: [ tmp3
				do: [ :arg2 | 
					[ stream := ZnCharacterReadWriteStream
						on: arg2 writeStream binary
						encoding: 'utf8'.
					^ self ]
						on: Error
						do: [  ] ] ].
	tmp3
		do: [ :arg3 | 
			[ stream := arg3 readStream.
			^ self ]
				on: Error
				do: [  ] ]
]
