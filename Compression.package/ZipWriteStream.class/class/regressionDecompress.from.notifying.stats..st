regression test
regressionDecompress: aFile from: tempFile notifying: progressBar stats: stats
	"Validate aFile as decompressed from tempFile"

	| unzip rawSize compressedSize buffer1 buffer2 |
	rawSize := aFile size.
	compressedSize := tempFile size.
	aFile ascii.
	aFile position: 0.
	tempFile ascii.
	tempFile position: 0.
	buffer1 := ByteArray new: 4096.
	buffer2 := buffer1 copy.
	unzip := FastInflateStream on: tempFile.
	[ aFile atEnd ]
		whileFalse: [ 
			progressBar current: aFile position.
			buffer1 := aFile nextInto: buffer1.
			buffer2 := unzip nextInto: buffer2.
			buffer1 = buffer2
				ifFalse: [ ^ self logProblem: 'contents ' for: aFile ] ].
	unzip next ifNotNil: [ ^ self logProblem: 'EOF' for: aFile ].
	stats at: #rawSize put: (stats at: #rawSize ifAbsent: [ 0 ]) + rawSize.
	stats at: #compressedSize put: (stats at: #compressedSize ifAbsent: [ 0 ]) + compressedSize.
	^ compressedSize asFloat / rawSize asFloat