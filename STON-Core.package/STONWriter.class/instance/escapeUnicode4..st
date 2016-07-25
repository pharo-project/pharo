private
escapeUnicode4: codePoint
	writeStream nextPutAll: '\u'.
	codePoint printOn: writeStream base: 16 nDigits: 4