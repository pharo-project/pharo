private - crlf
convertStringFromCr: aString 
	| inStream outStream |
	lineEndConvention ifNil: [^ aString].
	lineEndConvention == #cr ifTrue: [^ aString].
	lineEndConvention == #lf ifTrue: [^ aString copy replaceAll: Cr with: Lf].
	"lineEndConvention == #crlf"
	inStream := aString readStream.
	outStream :=  (String new: aString size) writeStream.
	[inStream atEnd]
		whileFalse: 
			[outStream nextPutAll: (inStream upTo: Cr).
			(inStream atEnd not or: [aString last = Cr])
				ifTrue: [outStream nextPutAll: CrLf]].
	^ outStream contents