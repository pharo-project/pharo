"
I am a new file. I support different kinds usages:
1) File stream support.
2) File read/write with explicit file offset.
3) Memory mapping.

"
Class {
	#name : 'NewFile',
	#superclass : 'FFIExternalObject',
	#instVars : [
		'mode'
	],
	#pools : [
		'NewFileConstants'
	],
	#category : 'NewFiles',
	#package : 'NewFiles'
}

{ #category : 'as yet unclassified' }
NewFile class >> apiName [
	^ 'NewFile'
]

{ #category : 'file primitives' }
NewFile class >> deleteFile: path [
	^ self primitiveDeleteFile: path
]

{ #category : 'public' }
NewFile class >> exists: aPath [
	| file |
	file := self openReadOnly: aPath.
	file ifNil: [ ^ false ].
	file close.
	^ true
]

{ #category : 'finalization' }
NewFile class >> finalizeResourceData: aHandle [
	aHandle isNull ifTrue: [ ^ self ].
	self primitiveClose: aHandle.
	aHandle beNull
]

{ #category : 'file open' }
NewFile class >> open: path mode: mode creationDisposition: creationDisposition flags: flags [
	| handle file |
	handle := self primitiveOpen: path mode: mode creationDisposition: creationDisposition flags: flags.
	handle isNull ifTrue: [ ^ nil ].

	file := self fromHandle: handle.
	
	file mode: mode.
	^ file autoRelease
]

{ #category : 'file open' }
NewFile class >> openAlways: path [
	^ self open: path mode: NewFileOpenModeReadWrite creationDisposition: NewFileCreationDispositionOpenAlways flags: NewFileOpenFlagsNone
]

{ #category : 'file open' }
NewFile class >> openCreateAlways: path [
	^ self open: path mode: NewFileOpenModeReadWrite creationDisposition: NewFileCreationDispositionCreateAlways flags: NewFileOpenFlagsNone
]

{ #category : 'file open' }
NewFile class >> openCreateNew: path [
	^ self open: path mode: NewFileOpenModeReadWrite creationDisposition: NewFileCreationDispositionCreateNew flags: NewFileOpenFlagsNone
]

{ #category : 'file open' }
NewFile class >> openExisting: path [
	^ self open: path mode: NewFileOpenModeReadWrite creationDisposition: NewFileCreationDispositionOpenExisting flags: NewFileOpenFlagsNone
]

{ #category : 'file open' }
NewFile class >> openReadOnly: path [
	^ self open: path mode: NewFileOpenModeReadOnly creationDisposition: NewFileCreationDispositionOpenExisting flags: NewFileOpenFlagsNone
]

{ #category : 'file open' }
NewFile class >> openTruncateExisting: path [
	^ self open: path mode: NewFileOpenModeReadWrite creationDisposition: NewFileCreationDispositionTruncateExisting flags: NewFileOpenFlagsNone
]

{ #category : 'file primitives' }
NewFile class >> primitiveClose: aFile [
	<primitive: 'primitiveFileClose' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'file primitives' }
NewFile class >> primitiveDeleteFile: path [
	<primitive: 'primitiveDeleteFile' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'file primitives' }
NewFile class >> primitiveIsAvailable [
	<primitive: 'primitiveIsAvailable' module: 'NewFilePlugin'>
	^ false
	
]

{ #category : 'file primitives' }
NewFile class >> primitiveOpen: path mode: mode creationDisposition: creationDisposition flags: flags [
	<primitive: 'primitiveFileOpen' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'accessing' }
NewFile >> atEnd [
	^ self position >= self size
]

{ #category : 'accessing' }
NewFile >> binaryStream [
	^ NewBinaryFileStream forFile: self forWrite: self isForWrite
]

{ #category : 'initialization' }
NewFile >> close [
	handle isNull ifTrue: [ ^ self ].
	self primitiveClose: handle.
	handle beNull
]

{ #category : 'file api' }
NewFile >> contents [
	| buffer |
	buffer := ByteArray new: self size.
	self readInto: buffer at: 0.
	^ buffer
]

{ #category : 'testing' }
NewFile >> isForWrite [
	^ mode = NewFileOpenModeReadWrite
]

{ #category : 'memory mapping' }
NewFile >> memoryMapWithProtection: protection [
	| mappingAddress |
	mappingAddress := self primitiveMemoryMap: handle withProtection: protection.
	mappingAddress isNull ifTrue: [ ^ nil ].

	^ NewFileMemoryMappingView new
		address: mappingAddress;
		file: self;
		size: self size;
		protection: protection;
		yourself
]

{ #category : 'accessing' }
NewFile >> mode [

	^ mode
]

{ #category : 'accessing' }
NewFile >> mode: anObject [

	mode := anObject
]

{ #category : 'accessing' }
NewFile >> position [
	^ self tell
]

{ #category : 'accessing' }
NewFile >> position: aPosition [
	^ self seek: aPosition mode: NewFileSeekModeSet
]

{ #category : 'primitives' }
NewFile >> primitive: aHandle readInto: buffer offset: bufferOffset withSize: readSize [
	<primitive: 'primitiveFileReadInto' module: 'NewFilePlugin'>
	^ self primitiveFailed
]

{ #category : 'primitives' }
NewFile >> primitive: aHandle readInto: buffer offset: bufferOffset withSize: readSize at: fileOffset [
	<primitive: 'primitiveFileReadIntoAtFileOffset' module: 'NewFilePlugin'>
	^ self primitiveFailed
]

{ #category : 'primitives' }
NewFile >> primitive: aHandle seek: offset mode: seekMode [
	<primitive: 'primitiveFileSeek' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'primitives' }
NewFile >> primitive: aHandle writeFrom: buffer offset: bufferOffset withSize: bufferSize [
	<primitive: 'primitiveFileWriteFrom' module: 'NewFilePlugin'>
	^ self primitiveFailed
]

{ #category : 'primitives' }
NewFile >> primitive: aHandle writeFrom: buffer offset: bufferOffset withSize: bufferSize at: fileOffset [
	<primitive: 'primitiveFileWriteFromAtFileOffset' module: 'NewFilePlugin'>
	^ self primitiveFailed
]

{ #category : 'primitives' }
NewFile >> primitiveClose: aFileHandle [
	<primitive: 'primitiveFileClose' module: 'NewFilePlugin'>
	^ self primitiveFailed 
	
]

{ #category : 'primitives' }
NewFile >> primitiveGetSize: handle [
	<primitive: 'primitiveFileGetSize' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'primitives' }
NewFile >> primitiveMemoryMap: handle withProtection: protection [
	<primitive: 'primitiveMemoryMapWithProtection' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'primitives' }
NewFile >> primitiveMemoryUnmap: handle [
	<primitive: 'primitiveMemoryUnmap' module: 'NewFilePlugin'>
	^ self primitiveFailed 
]

{ #category : 'primitives' }
NewFile >> primitiveTell: handle [
	<primitive: 'primitiveFileTell' module: 'NewFilePlugin'>
	^ self primitiveFailed
]

{ #category : 'primitives' }
NewFile >> primitiveTruncate: aHandle toSize: newSize [
	<primitive: 'primitiveFileTruncate' module: 'NewFilePlugin'>
	^ self primitiveFailed
]

{ #category : 'file api' }
NewFile >> readInto: buffer [
	^ self readInto: buffer offset: 0 withSize: buffer size
]

{ #category : 'file api' }
NewFile >> readInto: buffer at: fileOffset [
	^ self readInto: buffer offset: 0 withSize: buffer size at: fileOffset
]

{ #category : 'file api' }
NewFile >> readInto: buffer offset: bufferOffset withSize: readSize [
	^ self primitive: handle readInto: buffer offset: bufferOffset withSize: readSize
]

{ #category : 'file api' }
NewFile >> readInto: buffer offset: bufferOffset withSize: readSize at: fileOffset [
	^ self primitive: handle readInto: buffer offset: bufferOffset withSize: readSize at: fileOffset
]

{ #category : 'file api' }
NewFile >> readInto: buffer withSize: readSize [
	^ self readInto: buffer offset: 0 withSize: readSize
]

{ #category : 'memory mapping' }
NewFile >> readOnlyMemoryMap [
	^ self memoryMapWithProtection: NewFileMemMapProtectionReadOnly
]

{ #category : 'memory mapping' }
NewFile >> readWriteMemoryMap [
	^ self memoryMapWithProtection: NewFileMemMapProtectionReadWrite
]

{ #category : 'file api' }
NewFile >> seek: offset mode: seekMode [
	^ self primitive: handle seek: offset mode: seekMode
]

{ #category : 'file api' }
NewFile >> size [
	^ self primitiveGetSize: handle
]

{ #category : 'file api' }
NewFile >> tell [
	^ self primitiveTell: handle
]

{ #category : 'file api' }
NewFile >> truncate [
	^ self truncateToSize: 0
]

{ #category : 'file api' }
NewFile >> truncateToSize: newSize [
	^ self primitiveTruncate: handle toSize: newSize
]

{ #category : 'memory mapping' }
NewFile >> unmapView: aFileMemoryMappingView [
	self primitiveMemoryUnmap: handle.
	aFileMemoryMappingView unmapped
]

{ #category : 'file api' }
NewFile >> writeFrom: dataBuffer [
	^ self writeFrom: dataBuffer offset: 0 withSize: dataBuffer size
]

{ #category : 'file api' }
NewFile >> writeFrom: dataBuffer at: fileOffset [
	^ self writeFrom: dataBuffer offset: 0 withSize: dataBuffer size at: fileOffset
]

{ #category : 'file api' }
NewFile >> writeFrom: buffer offset: bufferOffset withSize: bufferSize [
	^ self primitive: handle writeFrom: buffer offset: bufferOffset withSize: bufferSize
]

{ #category : 'file api' }
NewFile >> writeFrom: buffer offset: bufferOffset withSize: bufferSize at: fileOffset [
	^ self primitive: handle writeFrom: buffer offset: bufferOffset withSize: bufferSize at: fileOffset
]

{ #category : 'file api' }
NewFile >> writeFrom: buffer withSize: bufferSize [
	^ self writeFrom: buffer offset: 0 withSize: bufferSize
]
