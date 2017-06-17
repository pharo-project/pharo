accessing
privateKey
	^ privateKey ifNil: [ privateKey := self class defaultPrivateKey ]