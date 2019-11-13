Extension { #name : #Class }

{ #category : #'*CodeExport' }
Class >> fileOut [
	"Create a file whose name is the name of the receiver with '.st' as the 
	extension, and file a description of the receiver onto it."
	
	| internalStream |
	internalStream := (String new: 100) writeStream.
	internalStream header; timeStamp.

	self hasSharedPools ifTrue: [
		self shouldFileOutPools
			ifTrue: [self fileOutSharedPoolsOn: internalStream]].
	self fileOutOn: internalStream.

	CodeExporter writeSourceCodeFrom: internalStream baseName: self name isSt: true
]

{ #category : #'*CodeExport' }
Class >> fileOutInitializerOn: aStream [
	^self class fileOutInitializerOn: aStream
]

{ #category : #'*CodeExport' }
Class >> fileOutOn: aFileStream [
	"File a description of the receiver on aFileStream."

	^ self fileOutOn: aFileStream initializing: true
]

{ #category : #'*CodeExport' }
Class >> fileOutOn: aFileStream initializing: aBool [
	"File a description of the receiver on aFileStream."

	SystemNotification signal: self name.
	super fileOutOn: aFileStream.
	self class nonTrivial
		ifTrue: [
			aFileStream cr; nextPutAll: '"-- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- "!'; cr; cr.
			self class
				fileOutOn: aFileStream
				initializing: aBool ]
]

{ #category : #'*CodeExport' }
Class >> fileOutPool: aPool onFileStream: aFileStream [ 
	| aPoolName |
	(aPool  isKindOf: SharedPool class) ifTrue:[^self notify: 'we do not fileout SharedPool type shared pools for now'].
	aPoolName := self environment keyAtIdentityValue: aPool.
	SystemNotification signal: aPoolName.
	aFileStream nextPutAll: 'Transcript show: ''' , aPoolName , '''; cr!'; cr.
	aFileStream nextPutAll: 'Smalltalk at: #' , aPoolName , ' put: Dictionary new!'; cr.
	aPool keys asSortedCollection do: [ :aKey | | aValue |
		aValue := aPool at: aKey.
		aFileStream nextPutAll: aPoolName , ' at: #''' , aKey asString , '''', ' put:  '.
		(aValue isKindOf: Number)
			ifTrue: [aValue printOn: aFileStream]
			ifFalse: [aFileStream nextPutAll: '('.
					aValue printOn: aFileStream.
					aFileStream nextPutAll: ')'].
		aFileStream nextPutAll: '!'; cr].
	aFileStream cr
]

{ #category : #'*CodeExport' }
Class >> fileOutSharedPoolsOn: aFileStream [
	"file out the shared pools of this class after prompting the user about each pool"
	| poolsToFileOut |
	poolsToFileOut := self sharedPools select: 
		[:aPool | (self shouldFileOutPool: (self environment keyAtIdentityValue: aPool))].
	poolsToFileOut do: [:aPool | self fileOutPool: aPool onFileStream: aFileStream].
	
]

{ #category : #'*CodeExport' }
Class >> shouldFileOutPool: aPoolName [
	"respond with true if the user wants to file out aPoolName"
	^self confirm: ('FileOut the sharedPool ', aPoolName, '?')
]

{ #category : #'*CodeExport' }
Class >> shouldFileOutPools [
	"respond with true if the user wants to file out the shared pools"
	^self confirm: 'FileOut selected sharedPools?'
]
