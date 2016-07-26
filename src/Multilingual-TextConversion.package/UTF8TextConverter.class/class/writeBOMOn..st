accessing
writeBOMOn: aStream
	"Write Byte Order Mark"
	aStream nextPut: 16rEF.
	aStream nextPut: 16rBB.
	aStream nextPut: 16rBF.
