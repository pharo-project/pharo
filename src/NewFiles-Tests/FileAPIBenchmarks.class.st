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
	
   SmallWriteBufferCount := 100000.
   SmallReadBufferCount := SmallWriteBufferCount.

	WriteBufferCount := 1000.
	ReadBufferCount := WriteBufferCount.

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

{ #category : 'as yet unclassified' }
FileAPIBenchmarks >> measure: aBlock withSampleCount: sampleCount title: title [
	| samples sampleIOSize sampleTime sampleSpeed average stdev |
	samples := Array new: sampleCount.
	1 to: sampleCount do: [ :sampleIndex |
		sampleIOSize := 0.
		sampleTime := [
			sampleIOSize := aBlock value: sampleIndex
		] microsecondsToRun * 1e-6.
		sampleSpeed := sampleIOSize asFloat / (sampleTime max: 1e-6) * 1e-6.
		
		samples at: sampleIndex put: sampleSpeed
	].

	average := samples average.
	stdev := samples stdev.
	Transcript show: title;
		show: ' '; show: sampleCount; show: ' samples';
		show: ' speed (MB/s): '; show: average; show: ' +- '; show: stdev; cr.
]

{ #category : 'writing' }
FileAPIBenchmarks >> readData [
	| readBuffer |
	readBuffer := ByteArray new: randomData size.
	file position: 0.
	self measure: [ :sampleIndex |
		| readCount |
		readCount := file readInto: readBuffer.
		self assert: readCount >= 0.
		readCount
	] withSampleCount: ReadBufferCount title: 'readData'

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
		self writeSmallData.
		self writeData.
		self readData.
	] ensure: [ 
		file close
	]

]

{ #category : 'writing' }
FileAPIBenchmarks >> writeData [
	file position: 0.
	self measure: [ :sampleIndex |
		| writeCount |
		writeCount := file writeFrom: randomData.
		self assert: writeCount >= 0.
		writeCount
	] withSampleCount: WriteBufferCount title: 'writeData'
]

{ #category : 'writing' }
FileAPIBenchmarks >> writeDataAtRandomOffsets [
	self measure: [ :sampleIndex |
		| writeCount |
		file position: (randomOffsets at: sampleIndex).
		writeCount := file writeFrom: smallRandomData.
		self assert: writeCount >= 0.
		writeCount
	] withSampleCount: randomOffsets size title: 'writeDataAtRandomOffsets'.

]

{ #category : 'writing' }
FileAPIBenchmarks >> writeSmallData [
	file position: 0.
	self measure: [ :sampleIndex |
		| writeCount |
		file position: (randomOffsets at: sampleIndex).
		writeCount := file writeFrom: smallRandomData.
		self assert: writeCount >= 0.
		writeCount
	] withSampleCount: SmallWriteBufferCount title: 'writeSmallData'.
]
