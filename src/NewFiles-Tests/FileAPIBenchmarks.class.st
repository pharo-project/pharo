"
I hold different benchmarks for testing the old file plugin vs the new file plugin.
"
Class {
	#name : 'FileAPIBenchmarks',
	#superclass : 'Object',
	#instVars : [
		'fileAPI',
		'file',
		'random',
		'randomData',
		'smallRandomData',
		'randomOffsets'
	],
	#classVars : [
		'RandomDataSize',
		'ReadBufferCount',
		'SmallRandomDataSize',
		'SmallReadBufferCount',
		'SmallWriteBufferCount',
		'WriteBufferCount'
	],
	#category : 'NewFiles-Tests',
	#package : 'NewFiles-Tests'
}

{ #category : 'class initialization' }
FileAPIBenchmarks class >> initialize [
	RandomDataSize := 1000000.
	SmallRandomDataSize := 2048.
	
   SmallWriteBufferCount := 10000.
   SmallReadBufferCount := SmallWriteBufferCount.

	WriteBufferCount := 200.
	ReadBufferCount := WriteBufferCount := 200..

]

{ #category : 'as yet unclassified' }
FileAPIBenchmarks class >> runWithFileAPI: aFileAPI [
	^ self new fileAPI: aFileAPI; run; yourself
]

{ #category : 'accessing' }
FileAPIBenchmarks >> fileAPI [

	^ fileAPI
]

{ #category : 'accessing' }
FileAPIBenchmarks >> fileAPI: anObject [

	fileAPI := anObject
]

{ #category : 'as yet unclassified' }
FileAPIBenchmarks >> generateRandomData [
	random := Random seed: 42.

	randomData := ByteArray new: RandomDataSize.
	1 to: RandomDataSize do: [ :index |
		randomData at: index put: (random nextInteger: 255)
	].

	smallRandomData := ByteArray new: SmallRandomDataSize.
	1 to: SmallRandomDataSize do: [ :index |
		smallRandomData at: index put: (random nextInteger: 255)
	].
	
	randomOffsets := IntegerArray new: SmallWriteBufferCount.
	1 to: SmallWriteBufferCount do: [ :i |
		randomOffsets at: i put: (random nextInteger: RandomDataSize // 10)
	].
]

{ #category : 'writing' }
FileAPIBenchmarks >> readData [
	| readTime readSize readBuffer readingSpeed |
	readSize := 0.
	readBuffer := ByteArray new: randomData size.
	readTime := [
		file position: 0.
		1 to: ReadBufferCount do: [ :writeIndex |
			| readCount |
			readCount := file readInto: readBuffer.
			readCount >= 0 ifTrue: [
				readSize := readSize + readCount. 
			]
		].
	] timeToRun asMicroseconds * 1e-6.

	readingSpeed := readSize asFloat / readTime * 1e-6.
	Transcript show: 'readData speed (MB/s) '; show: readingSpeed asString; cr.
]

{ #category : 'running' }
FileAPIBenchmarks >> run [
	| randomDatasetGenerationTime |
	file := fileAPI openCreateAlways: fileAPI apiName , '-Benchmarks.bin'.
	[
		Transcript show: fileAPI apiName; show: ' Benchmarks'; cr.

		randomDatasetGenerationTime := [ self generateRandomData ] timeToRun asMicroseconds * 0.001.
		Transcript show: 'RandomDatasetGenerationTime '; show: randomDatasetGenerationTime asString; show: ' ms'; cr.
		
		self writeDataAtRandomOffsets.
		self writeDataAtRandomOffsets.
		self writeSmallData.
		self writeSmallData.
		self writeData.
		self writeData.
		self readData.
		self readData.
	] ensure: [ 
		file close
	]

]

{ #category : 'writing' }
FileAPIBenchmarks >> writeData [
	| writeTime writeSize writingSpeed |
	writeSize := 0.
	writeTime := [
		file position: 0.
		1 to: WriteBufferCount do: [ :writeIndex |
			| writeCount |
			writeCount := file writeFrom: randomData.
			writeCount >= 0 ifTrue: [ 
				writeSize := writeSize + writeCount. 
			]
		].
	] timeToRun asMicroseconds * 1e-6.

	writingSpeed := writeSize asFloat / writeTime * 1e-6.
	Transcript show: 'writeData speed (MB/s) '; show: writingSpeed asString; cr.
]

{ #category : 'writing' }
FileAPIBenchmarks >> writeDataAtRandomOffsets [
	| writeTime writeSize writingSpeed |
	writeSize := 0.
	writeTime := [
		randomOffsets do: [ :position |
			| writeCount |
			file position: position.
			writeCount := file writeFrom: smallRandomData.
			writeCount >= 0 ifTrue: [ 
				writeSize := writeSize + writeCount. 
			]
		].
	] timeToRun asMicroseconds * 1e-6.

	writingSpeed := writeSize asFloat / writeTime * 1e-6.
	Transcript show: 'writeDataAtRandomOffsets speed (MB/s) '; show: writingSpeed asString; cr.
]

{ #category : 'writing' }
FileAPIBenchmarks >> writeSmallData [
	| writeTime writeSize writingSpeed |
	writeSize := 0.
	writeTime := [
		file position: 0.
		1 to: SmallWriteBufferCount do: [ :writeIndex |
			file writeFrom: smallRandomData.
			writeSize := writeSize + smallRandomData size. 
		].
	] timeToRun asMicroseconds * 1e-6.

	writingSpeed := writeSize asFloat / writeTime * 1e-6.
	Transcript show: 'writeSmallData speed (MB/s) '; show: writingSpeed asString; cr.
]
