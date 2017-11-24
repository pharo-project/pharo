"
I serialize or materialize objects using STON framework. Create instance of me calling GTEventPacking ston.
"
Class {
	#name : #GTStonPacking,
	#superclass : #GTEventPacking,
	#category : #'GT-EventRecorder-Support'
}

{ #category : #private }
GTStonPacking >> materialize: aByteArray [
	| stream gzstream |
	stream := aByteArray asString readStream.
	gzstream := GZipReadStream on: stream.
	^ [ STON fromStream: gzstream ] ensure: [ gzstream close. stream close ]
]

{ #category : #private }
GTStonPacking >> serialize: anObject [
	^ (String streamContents: [ :stream | | gzstream |
		gzstream := GZipWriteStream on: stream.
		[ STON put: anObject onStreamPretty: gzstream ]
			ensure: [ gzstream close ] ]) asByteArray
]

{ #category : #accessing }
GTStonPacking >> technologyName [
	^ 'ston'
]
