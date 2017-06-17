accessing
publicKey
	^ publicKey ifNil: [ publicKey := self class defaultPublicKey ]