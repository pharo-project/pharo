Class {
	#name : 'SourceFileWriter',
	#superclass : 'Object',
	#instVars : [
		'metadata',
		'prior',
		'timestamp',
		'sourceFile',
		'successBlock',
		'failBlock'
	],
	#category : 'System-Sources-Sources',
	#package : 'System-Sources',
	#tag : 'Sources'
}

{ #category : 'accessing' }
SourceFileWriter >> exporterClass [

	^ sourceFile exporterClass
]

{ #category : 'accessing' }
SourceFileWriter >> failBlock [

	^ failBlock ifNil: [ [ "Do nothing" ] ]
]

{ #category : 'accessing' }
SourceFileWriter >> failBlock: anObject [

	failBlock := anObject
]

{ #category : 'accessing' }
SourceFileWriter >> metadata [

	^ metadata ifNil: [ '' ]
]

{ #category : 'accessing' }
SourceFileWriter >> metadata: anObject [

	metadata := anObject
]

{ #category : 'accessing' }
SourceFileWriter >> prior [

	^ prior
]

{ #category : 'accessing' }
SourceFileWriter >> prior: anObject [

	prior := anObject
]

{ #category : 'accessing' }
SourceFileWriter >> sourceFile [

	^ sourceFile
]

{ #category : 'accessing' }
SourceFileWriter >> sourceFile: anObject [

	sourceFile := anObject
]

{ #category : 'accessing' }
SourceFileWriter >> successBlock [

	^ successBlock ifNil: [ [ :sourcePointer | "Do nothing" ] ]
]

{ #category : 'accessing' }
SourceFileWriter >> successBlock: anObject [

	successBlock := anObject
]

{ #category : 'accessing' }
SourceFileWriter >> timestamp [

	^ timestamp
]

{ #category : 'accessing' }
SourceFileWriter >> timestamp: anObject [

	timestamp := anObject
]

{ #category : 'writing' }
SourceFileWriter >> writeClassDefinition: aClassDefinition [

	sourceFile
		writeChangesDuring: [ :file |
				| position |
				position := file position.
				(self exporterClass onStream: file) writeClassDefinition: aClassDefinition.
				position ]
		onSuccess: self successBlock
		onFail: self failBlock
]

{ #category : 'writing' }
SourceFileWriter >> writeComment: aComment ofClass: aClass [

	sourceFile
		writeChangesDuring: [ :file |
				(self exporterClass onStream: file)
					writeComment: aComment
					ofClass: aClass
					stamp: timestamp
					withMetadata: self metadata
					priorSourcePointer: prior ]
		onSuccess: self successBlock
		onFail: self failBlock
]

{ #category : 'writing' }
SourceFileWriter >> writeSource: source ofMethod: method protocol: protocol [

	sourceFile
		writeChangesDuring: [ :file |
				(self exporterClass onStream: file)
					writeMethodWithClass: method methodClass
					selector: method selector
					source: source
					protocol: protocol
					stamp: timestamp
					withMetadata: self metadata
					priorSourcePointer: prior ]
		onSuccess: self successBlock
		onFail: self failBlock
]
