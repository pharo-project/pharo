*Multilingual-TextConversion
convertFromWithConverter: converter

	| readStream c |
	readStream := self readStream.
	^ self class new: self size streamContents: [ :writeStream|
		converter ifNil: [^ self].
		[readStream atEnd] whileFalse: [
			c := converter nextFromStream: readStream.
			c
				ifNotNil: [writeStream nextPut: c] 
				ifNil: [^ writeStream contents]]].
