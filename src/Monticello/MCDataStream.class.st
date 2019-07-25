"
This is the save-to-disk facility. A DataStream can store one or more objects in a persistent form.

To handle objects with sharing and cycles, you must use a
ReferenceStream instead of a DataStream.  (Or SmartRefStream.)  ReferenceStream is typically
faster and produces smaller files because it doesn't repeatedly write the same Symbols.

Here is the way to use DataStream and ReferenceStream:
	rr := ReferenceStream fileNamed: 'test.obj'.
	rr nextPut: <your object>.
	rr close.

To get it back:
	rr := ReferenceStream fileNamed: 'test.obj'.
	<your object> := rr next.
	rr close.

Each object to be stored has two opportunities to control what gets stored.  On the high level, objectToStoreOnDataStream allows you to substitute another object on the way out.  The low level hook is storeDataOn:. The read-in counterparts to these messages are comeFullyUpOnReload and (class) readDataFrom:size:. See these methods for more information about externalizing and internalizing.

NOTE: A DataStream should be treated as a write-stream for writing.  It is a read-stream for reading.  It is not a ReadWriteStream.

"
Class {
	#name : #MCDataStream,
	#superclass : #Stream,
	#instVars : [
		'byteStream',
		'topCall',
		'basePos'
	],
	#classVars : [
		'ReadSelectors',
		'TypeMap',
		'WriteSelectors'
	],
	#category : #'Monticello-Storing'
}

{ #category : #cleanup }
MCDataStream class >> cleanUp [
	"Re-initialize DataStream to avoid hanging onto obsolete classes"

	self initialize
]

{ #category : #'instance creation' }
MCDataStream class >> detectFile: aBlock do: anotherBlock [

	^aBlock value
		ifNil: [nil]
      	ifNotNil: [:file| [anotherBlock value: file] ensure: [file close]]
]

{ #category : #'instance creation' }
MCDataStream class >> fileNamed: aString [
	"Here is the way to use DataStream and ReferenceStream:

| rr |
rr := ReferenceStream fileNamed: 'test.obj'.
rr nextPut: 'Zork'.
rr close.
"

	^ self on: (FileStream fileNamed: aString)
]

{ #category : #'instance creation' }
MCDataStream class >> fileNamed: fileName do: aBlock [
	"Returns the result of aBlock."
	
	^ self detectFile: [ self fileNamed: fileName ] do: aBlock
]

{ #category : #'initialize-release' }
MCDataStream class >> initialize [
	self initializeTypeMap 
]

{ #category : #'initialize-release' }
MCDataStream class >> initializeTypeMap [
	"TypeMap maps Smalltalk classes to type ID numbers which identify the data stream primitive formats.  nextPut: writes these IDs to the data stream.  NOTE: Changing these type ID numbers will invalidate all extant data stream files.  Adding new ones is OK.  
	Classes named here have special formats in the file.  If such a class has a subclass, it will use type 9 and write correctly.  It will just be slow.  (Later write the class name in the special format, then subclasses can use the type also.)
	 See nextPut:, next, typeIDFor:, & ReferenceStream>>isAReferenceType:"

	"MCDataStream initialize"

	TypeMap := WeakKeyDictionary new: 80.	"sparse for fast hashing"
	ReadSelectors := WeakValueDictionary new: 80.
	WriteSelectors := WeakValueDictionary new: 80.

	"These are all the type ids used in serialization and deserealization. The Missing types id are optional for Monticello, see package 
	Monticello-OldDataStreamCompatibility for the registering of the older types ids."
	self
		registerClass: UndefinedObject
		atIndex: 1
		usingReadSelector: #readNil
		usingWriteSelector: #writeNil:.
	self
		registerClass: True
		atIndex: 2
		usingReadSelector: #readTrue
		usingWriteSelector: #writeTrue:.
	self
		registerClass: False
		atIndex: 3
		usingReadSelector: #readFalse
		usingWriteSelector: #writeFalse:.
	self
		registerClass: SmallInteger
		atIndex: 4
		usingReadSelector: #readInteger
		usingWriteSelector: #writeInteger:.
	self
		registerClass: ByteSymbol
		atIndex: 6
		usingReadSelector: #readSymbol
		usingWriteSelector: #writeSymbol:.
	self
		registerClass: Array
		atIndex: 8
		usingReadSelector: #readArray
		usingWriteSelector: #writeArray:.
	"Type id 9 is special."
	self registerReaderSelector: #readInstance atIndex: 9.
	self registerWriterSelector: #writeInstance: atIndex: 9.
	
	self
		registerClass: ByteString
		atIndex: 17
		usingReadSelector: #readString
		usingWriteSelector: #writeString:.
		
	self registerClass: WideString
		atIndex: 20
		usingReadSelector: #readWordLike
		usingWriteSelector: #writeWordLike:
]

{ #category : #'instance creation' }
MCDataStream class >> new [
	^ self basicNew
]

{ #category : #'instance creation' }
MCDataStream class >> newFileNamed: aString [
	"Here is the way to use DataStream and ReferenceStream:
	|rr|
	rr := ReferenceStream fileNamed: 'test.obj'.
	rr nextPut: 'Zork'.
	rr close.
"

	^ self on: (FileStream newFileNamed: aString)
]

{ #category : #'instance creation' }
MCDataStream class >> oldFileNamed: aString [
	"Here is the way to use DataStream and ReferenceStream:

	|rr |
	rr := ReferenceStream oldFileNamed: 'test.obj'.
	^ rr nextAndClose.
"

	| strm ff |
	ff := FileStream oldFileOrNoneNamed: aString.
	ff ifNil: [^ nil].
	strm := self on: (ff binary).
	^ strm
]

{ #category : #'instance creation' }
MCDataStream class >> on: aStream [
	"Open a new DataStream onto a low-level I/O stream."

	^ self basicNew setStream: aStream
		"aStream binary is in setStream:"

]

{ #category : #'instance creation' }
MCDataStream class >> readOnlyFileNamed:  aString [
	"Here is the way to use DataStream and ReferenceStream:
	
	|rr|
	rr := ReferenceStream fileNamed: 'test.obj'.
	rr nextPut: 'Zork'.
	rr close.
"

	^ self on: (FileStream readOnlyFileNamed: aString)
]

{ #category : #'initialize-release' }
MCDataStream class >> readSelectors [
	ReadSelectors ifNil: [ self initializeTypeMap ].
	^ReadSelectors 
]

{ #category : #'initialize-release' }
MCDataStream class >> registerClass: aClass atIndex: anIndex usingReadSelector: readSelector usingWriteSelector: writeSelector [
	self typeMap at: aClass put: anIndex.
	self readSelectors at: anIndex put: readSelector.
	self writeSelectors at: anIndex put: writeSelector
]

{ #category : #'initialize-release' }
MCDataStream class >> registerReaderSelector: aSelector atIndex: index [
	self readSelectors at: index put: aSelector
]

{ #category : #'initialize-release' }
MCDataStream class >> registerWriterSelector: aSelector atIndex: anIndex [
	self writeSelectors at: anIndex put: aSelector
]

{ #category : #'instance creation' }
MCDataStream class >> streamedRepresentationOf: anObject [

	| file |
	file := (RWBinaryOrTextStream on: (ByteArray new: 5000)).
	file binary.
	(self on: file) nextPut: anObject.
	^file contents
]

{ #category : #'initialize-release' }
MCDataStream class >> typeMap [
	TypeMap ifNil: [ self initializeTypeMap ].
	^ TypeMap
]

{ #category : #'instance creation' }
MCDataStream class >> unStream: aString [

	^(self on: ((RWBinaryOrTextStream with: aString) reset; binary)) next
]

{ #category : #'initialize-release' }
MCDataStream class >> writeSelectors [
	WriteSelectors ifNil: [ self initializeTypeMap ].
	^ WriteSelectors
]

{ #category : #other }
MCDataStream >> atEnd [
    "Answer true if the stream is at the end."

    ^ byteStream atEnd
]

{ #category : #'write and read' }
MCDataStream >> beginInstance: aClass size: anInteger [
	"This is for use by storeDataOn: methods.
	 Cf. Object>>storeDataOn:."

		"Addition of 1 seems to make extra work, since readInstance
		has to compensate.  Here for historical reasons dating back
		to Kent Beck's original implementation in late 1988.

		In ReferenceStream, class is just 5 bytes for shared symbol.

		SmartRefStream puts out the names and number of class's instances variables for checking."

	byteStream nextNumber: 4 put: anInteger + 1.

	self nextPut: aClass name
]

{ #category : #'write and read' }
MCDataStream >> beginReference: anObject [
    "We're starting to read anObject. Remember it and its reference
     position (if we care; ReferenceStream cares). Answer the
     reference position."

    ^ 0
]

{ #category : #other }
MCDataStream >> byteStream [
	^ byteStream
]

{ #category : #other }
MCDataStream >> close [
	"Close the stream."

	| bytes |
	byteStream closed 
		ifFalse: [
			bytes := byteStream position.
			byteStream close]
		ifTrue: [bytes := 'unknown'].
	^ bytes
]

{ #category : #other }
MCDataStream >> contents [
	^byteStream contents
]

{ #category : #other }
MCDataStream >> errorWriteReference: anInteger [
    "PRIVATE -- Raise an error because this case of nextPut:'s perform:
     shouldn't be called."

    self error: 'This should never be called'
]

{ #category : #other }
MCDataStream >> flush [
    "Guarantee that any writes to me are actually recorded on disk."

    ^ byteStream flush
]

{ #category : #'write and read' }
MCDataStream >> getCurrentReference [
    "PRIVATE -- Return the currentReference posn.
     Overridden by ReferenceStream."

    ^ 0
]

{ #category : #'write and read' }
MCDataStream >> maybeBeginReference: internalObject [
	"Do nothing.  See ReferenceStream|maybeBeginReference:"

	^ internalObject
]

{ #category : #'write and read' }
MCDataStream >> next [
	"Answer the next object in the stream."
	| type selector anObject  internalObject |

	type := byteStream next.
	type ifNil: [
		byteStream close.	"clean up"
		byteStream position = 0 
			ifTrue: [self error: 'The file did not exist in this directory'] 
			ifFalse: [self error: 'Unexpected end of object file'].
		^ nil].
	type = 0 ifTrue: [
		byteStream close.	"clean up"
		self error: 'Expected start of object, but found 0'.
		^ nil].

	selector := self class readSelectors at: type 
		ifAbsent:[ 
				byteStream close.
				self error: 'Unrecognised type id. You should load the Monticello-OldDataStreamCompatibility package'
		].
	
	anObject := self perform: selector. "A method that recursively
		calls next (readArray, readInstance, objectAt:) must save &
		restore the current reference position."

		"After reading the externalObject, internalize it.
		 #readReference is a special case. Either:
		   (1) We actually have to read the object, recursively calling
			   next, which internalizes the object.
		   (2) We just read a reference to an object already read and
			   thus already interalized.
		 Either way, we must not re-internalize the object here."
	selector == #readReference ifTrue: [^ anObject].
	internalObject := anObject comeFullyUpOnReload: self.
	^ self maybeBeginReference: internalObject
]

{ #category : #other }
MCDataStream >> next: anInteger [
    "Answer an Array of the next anInteger objects in the stream."
    | array |

    array := Array new: anInteger.
    1 to: anInteger do: [:i |
        array at: i put: self next].
    ^ array
]

{ #category : #other }
MCDataStream >> nextAndClose [
	"Speedy way to grab one object.  Only use when we are inside an object binary file.  Do not use for the start of a SmartRefStream mixed code-and-object file."

	| obj |
	obj := self next.
	self close.
	^ obj
]

{ #category : #'write and read' }
MCDataStream >> nextPut: anObject [
	"Write anObject to the receiver stream. Answer anObject."
	| typeID selector objectToStore |

	typeID := self typeIDFor: anObject.
	(self tryToPutReference: anObject typeID: typeID)
		ifTrue: [^ anObject].

	objectToStore := (self objectIfBlocked: anObject).
	objectToStore == anObject ifFalse: [typeID := self typeIDFor: objectToStore].

	byteStream nextPut: typeID.
	selector := self class writeSelectors at: typeID.
	self perform: selector with: objectToStore.

	^ anObject
]

{ #category : #'write and read' }
MCDataStream >> nextPutAll: aCollection [
    "Write each of the objects in aCollection to the
     receiver stream. Answer aCollection."

    ^ aCollection do: [:each | self nextPut: each]
]

{ #category : #'write and read' }
MCDataStream >> objectAt: anInteger [
	"PRIVATE -- Read & return the object at a given stream position.  08:18 tk  anInteger is a relative file position. "
	| savedPosn anObject refPosn |

	savedPosn := byteStream position.	"absolute"
	refPosn := self getCurrentReference.	"relative position"

	byteStream position: anInteger + basePos.	"was relative"
	anObject := self next.

	self setCurrentReference: refPosn.	"relative position"
	byteStream position: savedPosn.		"absolute"
	^ anObject
]

{ #category : #'write and read' }
MCDataStream >> objectIfBlocked: anObject [
	"We don't do any blocking"

	^ anObject
]

{ #category : #'write and read' }
MCDataStream >> outputReference: referencePosn [
	"PRIVATE -- Output a reference to the object at integer stream position referencePosn (relative to basePos). To output a weak reference to an object not yet written, supply (self vacantRef) for referencePosn."

	byteStream nextPut: 10. "reference typeID"
	byteStream nextNumber: 4 put: referencePosn	"relative position"
]

{ #category : #'write and read' }
MCDataStream >> readArray [
	"PRIVATE -- Read the contents of an Array.
	 We must do beginReference: here after instantiating the Array
	 but before reading its contents, in case the contents reference
	 the Array. beginReference: will be sent again when we return to
	 next, but that's ok as long as we save and restore the current
	 reference position over recursive calls to next."
	| count array refPosn |

	count := byteStream nextNumber: 4.

	refPosn := self beginReference: (array := Array new: count).		"relative pos"
	1 to: count do: [:i |
		array at: i put: self next].
	self setCurrentReference: refPosn.		"relative pos"
	^ array
]

{ #category : #'write and read' }
MCDataStream >> readFalse [
    "PRIVATE -- Read the contents of a False."

    ^ false
]

{ #category : #'write and read' }
MCDataStream >> readInstance [
	"PRIVATE -- Read the contents of an arbitrary instance.
	 ASSUMES: readDataFrom:size: sends me beginReference: after it
	   instantiates the new object but before reading nested objects.
	 NOTE: We must restore the current reference position after
	   recursive calls to next.
	Let the instance, not the class read the data.  "

	| instSize aSymbol refPosn anObject newClass |
	instSize := (byteStream nextNumber: 4) - 1.
	refPosn := self getCurrentReference.
	aSymbol := self next.
	newClass := Smalltalk globals at: aSymbol asSymbol.
	anObject := newClass isVariable
		ifFalse: [ newClass basicNew ]
		ifTrue: [ newClass basicNew: instSize - newClass instSize ].	"Create object here"
	self setCurrentReference: refPosn.	"before readDataFrom:size:"
	anObject := anObject readDataFrom: self size: instSize.
	self setCurrentReference: refPosn.	"before returning to next"
	^ anObject
]

{ #category : #'write and read' }
MCDataStream >> readInteger [
    "PRIVATE -- Read the contents of a SmallInteger."

    ^ byteStream nextInt32	"signed!!!"
]

{ #category : #'write and read' }
MCDataStream >> readNil [
    "PRIVATE -- Read the contents of an UndefinedObject."

    ^ nil
]

{ #category : #'write and read' }
MCDataStream >> readString [

	| str |
	byteStream ascii.
	str := byteStream nextString.
	byteStream binary.
	^ str

]

{ #category : #'write and read' }
MCDataStream >> readSymbol [
    "PRIVATE -- Read the contents of a Symbol."
    ^ self readString asSymbol
]

{ #category : #'write and read' }
MCDataStream >> readTrue [
    "PRIVATE -- Read the contents of a True."

    ^ true
]

{ #category : #initialization }
MCDataStream >> reset [
    "Reset the stream."

    byteStream reset
]

{ #category : #other }
MCDataStream >> rootObject [
	"Return the object at the root of the tree we are filing out.  "

	^ topCall
]

{ #category : #other }
MCDataStream >> rootObject: anObject [
	"Return the object at the root of the tree we are filing out.  "

	topCall := anObject
]

{ #category : #'write and read' }
MCDataStream >> setCurrentReference: refPosn [
    "PRIVATE -- Set currentReference to refPosn.
     Noop here. Cf. ReferenceStream."
]

{ #category : #other }
MCDataStream >> setStream: aStream [
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos := aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream := aStream.
]

{ #category : #other }
MCDataStream >> setStream: aStream reading: isReading [
	"PRIVATE -- Initialization method."

	aStream binary.
	basePos := aStream position.	"Remember where we start.  Earlier part of file contains a class or method file-in.  Allow that to be edited.  We don't deal in absolute file locations."
	byteStream := aStream.
]

{ #category : #other }
MCDataStream >> size [
    "Answer the stream's size."

    ^ byteStream size
]

{ #category : #'write and read' }
MCDataStream >> tryToPutReference: anObject typeID: typeID [
    "PRIVATE -- If we support references for type typeID, and if
       anObject already appears in my output stream, then put a
       reference to the place where anObject already appears. If we
       support references for typeID but didn't already put anObject,
       then associate the current stream position with anObject in
       case one wants to nextPut: it again.
     Return true after putting a reference; false if the object still
       needs to be put.
     For DataStream this is trivial. ReferenceStream overrides this."

    ^ false
]

{ #category : #'write and read' }
MCDataStream >> typeIDFor: anObject [
	"Return the typeID for anObject's class.  This is where the tangle of objects is clipped to stop everything from going out.  
	Classes can control their instance variables by defining objectToStoreOnDataStream.
	Any object in blockers is not written out.  See ReferenceStream.objectIfBlocked: and DataStream nextPut:.
	Morphs do not write their owners.  See Morph.storeDataOn:   Each morph tells itself to 'prepareToBeSaved' before writing out."
	
	^ self class typeMap at: anObject class ifAbsent: [9 "instance of any normal class"]	
"See DataStream initialize.  nil=1. true=2. false=3. a SmallInteger=4. (a String was 5). a Symbol=6.  a ByteArray=7. an Array=8. other = 9.  a Bitmap=11. a Metaclass=12. a Float=14.  a Rectangle=15. any instance that can have a short header=16.  a String=17 (new format). a WordArray=18."
]

{ #category : #other }
MCDataStream >> vacantRef [
	"Answer the magic 32-bit constant we use ***ON DISK*** as a stream 'reference
	 position' to identify a reference that's not yet filled in. This must be a
	 value that won't be used as an ordinary reference. Cf. outputReference: and
	 readReference. -- 
	 NOTE: We could use a different type ID for vacant-refs rather than writing
		object-references with a magic value. (The type ID and value are
		overwritten by ordinary object-references when weak refs are fullfilled.)"

	^ SmallInteger maxVal
]

{ #category : #'write and read' }
MCDataStream >> writeArray: anArray [
	"PRIVATE -- Write the contents of an Array."

	byteStream nextNumber: 4 put: anArray size.
	self nextPutAll: anArray.
]

{ #category : #'write and read' }
MCDataStream >> writeFalse: aFalse [
    "PRIVATE -- Write the contents of a False."
]

{ #category : #'write and read' }
MCDataStream >> writeInstance: anObject [
    "PRIVATE -- Write the contents of an arbitrary instance."

    ^ anObject storeDataOn: self
]

{ #category : #'write and read' }
MCDataStream >> writeInteger: anInteger [
	"PRIVATE -- Write the contents of a SmallInteger."

	byteStream nextInt32Put: anInteger	"signed!!!!!"
]

{ #category : #'write and read' }
MCDataStream >> writeNil: anUndefinedObject [
    "PRIVATE -- Write the contents of an UndefinedObject."
]

{ #category : #'write and read' }
MCDataStream >> writeString: aString [
	"PRIVATE -- Write the contents of a String."

	byteStream nextStringPut: aString.
]

{ #category : #'write and read' }
MCDataStream >> writeSymbol: aSymbol [
    "PRIVATE -- Write the contents of a Symbol."

    self writeString: aSymbol
]

{ #category : #'write and read' }
MCDataStream >> writeTrue: aTrue [
    "PRIVATE -- Write the contents of a True."
]
