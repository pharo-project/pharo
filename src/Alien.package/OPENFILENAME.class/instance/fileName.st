accessing
fileName
	^(Alien forPointer: (self unsignedLongAt: self lpstrFileOffset)) strcpyUTF8