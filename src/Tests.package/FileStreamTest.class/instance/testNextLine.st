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
	file := File openForReadFileNamed: filename.
	lines do: [ :e | self assert: file nextLine = e ].
	self assert: file nextLine isNil.
	file close ] ensure: [ 
		(FileSystem disk workingDirectory / filename) ensureDelete ].