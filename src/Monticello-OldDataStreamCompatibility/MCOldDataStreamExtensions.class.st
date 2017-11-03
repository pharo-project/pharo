"
This is an extension class for the MCDataStream, it should only used if you need backward compatibility. But we have tested that is only needed because is ""copy-pasted"" code. MC is not using at all the defined types id. 

This package is unloadeable.
"
Class {
	#name : #MCOldDataStreamExtensions,
	#superclass : #Object,
	#category : #Monticello-OldDataStreamCompatibility
}

{ #category : #'class initialization' }
MCOldDataStreamExtensions class >> initialize [
	MCDataStream registerReaderSelector: #readStringOld atIndex: 5.
	MCDataStream registerClass: ByteArray atIndex: 7  usingReadSelector: #readByteArray usingWriteSelector: #writeByteArray:.
	MCDataStream registerReaderSelector: #readReference atIndex: 10.
	MCDataStream registerClass: Bitmap atIndex: 11  usingReadSelector: #readBitmap usingWriteSelector: #writeBitmap:.
	" 12 is for Classes but never worked".
	MCDataStream registerReaderSelector: #readInstace atIndex: 13.
	MCDataStream registerClass: Float atIndex: 14  usingReadSelector: #readFloat usingWriteSelector: #writeFloat:.
	MCDataStream registerClass: Rectangle atIndex: 15  usingReadSelector: #readRectangle usingWriteSelector: #writeRectangle:.	
	MCDataStream registerReaderSelector: #readShortInst atIndex: 16.
	MCDataStream registerClass: WordArray atIndex: 18  usingReadSelector: #readWordArray usingWriteSelector: #writeBitmap:.	
	MCDataStream registerReaderSelector: #readMethod atIndex: 21.

	
]
