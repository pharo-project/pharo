Extension { #name : #FileReference }

{ #category : #'*ston-core' }
FileReference >> stonContainSubObjects [
	^ false
]

{ #category : #'*ston-core' }
FileReference >> stonOn: stonWriter [
	self fileSystem isDiskFileSystem
		ifTrue: [ | diskFilePath |
			diskFilePath := String streamContents: [ :out | 
				filesystem printPath: path on: out ].
			stonWriter 
				writeObject: self 
				named: STONFileReference stonName 
				listSingleton: diskFilePath ]
		ifFalse: [ 
			super stonOn: stonWriter ]
]
