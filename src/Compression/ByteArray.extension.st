Extension { #name : #ByteArray }

{ #category : #'*Compression' }
ByteArray >> lastIndexOfPKSignature: aSignature [
	"Answer the last index in me where aSignature (4 bytes long) occurs, or 0 if not found"
	| a b c d |
	a := aSignature first.
	b := aSignature second.
	c := aSignature third.
	d := aSignature fourth.
	(self size - 3) to: 1 by: -1 do: [ :i |
		(((self at: i) = a)
			and: [ ((self at: i + 1) = b)
				and: [ ((self at: i + 2) = c)
					and: [ ((self at: i + 3) = d) ]]])
						ifTrue: [ ^i ]
	].
	^0
]

{ #category : #'*Compression' }
ByteArray >> unzipped [
	"Assuming the receiver contains GZIP compressed data, 
	return a ByteArray with the decompressed data."
	
	"#[31 139 8 0 0 0 0 0 0 0 99 100 98 6 0 29 128 188 85 3 0 0 0] unzipped >>> #[1 2 3]"

	^ (GZipReadStream on: self) upToEnd
]

{ #category : #'*Compression' }
ByteArray >> zipped [
	"Return a ByteArray containing a GZIP compressed version of the receiver"
	
	"#[ 1 2 3 ] zipped >>> #[31 139 8 0 0 0 0 0 0 0 99 100 98 6 0 29 128 188 85 3 0 0 0]"

	^ ByteArray streamContents: [ :out | 
		(GZipWriteStream on: out)
			nextPutAll: self;
			close ]
]
