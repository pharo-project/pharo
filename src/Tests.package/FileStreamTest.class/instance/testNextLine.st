testing
testNextLine
	| filename lines text |
	filename := 'filestream.tst'.
	lines := #('line 1' ' and line 2' '' 'fourth').
	text := lines first , String cr , lines second , String crlf , lines third , String lf , lines fourth.
	[ 
	| file |
	(File named: filename)
		writeStreamDo: [ :stream | stream nextPutAll: text ].
	file := ZnFastLineReader on: 
			(ZnCharacterReadStream
				on: (File openForReadFileNamed: filename)
				encoding: 'utf8').
	lines do: [ :e | self assert: file nextLine equals: e ].
	self assert: file nextLine isNil.
	file close ] ensure: [ 
		(FileSystem disk workingDirectory / filename) ensureDelete ].