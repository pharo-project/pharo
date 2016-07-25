installing
fontNamed: fontName fromMimeLiteral: aString
	"This method allows a font set to be captured as sourcecode in a subclass.
	The string literals will presumably be created by printing, eg,
		(FileStream readOnlyFileNamed: 'Palatino24.sf2') contentsOfEntireFile,
		and following the logic in compileFont: to encode and add a heading.

	See the method installAsTextStyle to see how this can be used."

	^ StrikeFont new 
		name: fontName;
		readFromStrike2Stream: (Base64MimeConverter mimeDecodeToBytes: aString readStream)