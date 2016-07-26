private
readSignatureFrom: stream
	"Returns next signature from given stream, leaves stream positioned afterwards."

	| signatureData | 
	signatureData := ByteArray new: 4.
	stream next: 4 into: signatureData.
	({ CentralDirectoryFileHeaderSignature . LocalFileHeaderSignature . EndOfCentralDirectorySignature }
		includes: signatureData)
			ifFalse: [ ^self error: 'bad signature ', signatureData asString asHex, ' at position ', (stream position - 4) asString ].
	^signatureData
