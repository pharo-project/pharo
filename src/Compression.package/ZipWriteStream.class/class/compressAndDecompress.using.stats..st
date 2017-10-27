regression test
compressAndDecompress: aFile using: tempFile stats: stats

	| fileSize tempStream result |
	aFile ifNil: [^nil].
	fileSize := aFile size.
	(fileSize < 1"00000" "or:[fileSize > 1000000]") ifTrue:[aFile close. ^nil].
	Transcript cr; show:'Testing ', aFile name,' ... '.
	tempStream := File openForWriteFileNamed: tempFile fullName.
	'Compressing ', aFile name,'...' displayProgressFrom:  1 to: aFile size during: [ :bar |
			result := self regressionCompress: aFile into: tempStream notifiying: bar stats: stats].
	result ifTrue: [
		'Validating ', aFile name,'...' displayProgressFrom: 0 to: aFile size during: [ :bar |
				result := self regressionDecompress: aFile from: tempStream notifying: bar stats: stats]].
	aFile close.
	tempStream close.
	tempFile delete.
	result ~~ false ifTrue: [
		Transcript show:' ok (', (result * 100 truncateTo: 0.01) printString,')'].
	^result