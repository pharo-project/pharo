testing
testDetectFileDo
	| file |
	[ file := 'filestream.tst' asFileReference.
	file writeStreamDo: [ :s | s nextPutAll: '42' ].
	FileStream
		detectFile: [ file readStream ]
		do: [ :stream | 
			self assert: stream notNil.
			self deny: stream closed.
			self assert: stream contentsOfEntireFile = '42']]
		ensure: [
			file ensureDelete ].