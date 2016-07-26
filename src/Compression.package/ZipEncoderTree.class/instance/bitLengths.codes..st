private
bitLengths: blArray codes: codeArray
	bitLengths := blArray as: WordArray.
	codes := codeArray as: WordArray.
	[(self bitLengthAt: maxCode) > 0] assert.