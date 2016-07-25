accessing
md5Hash: aString
	| hash |
	"Answer hash of aString as lowercase 32 digit hex String."
	Smalltalk at: #MD5 ifPresent: [ :cls |
		^ ((cls new initialize) hashStream: (aString asByteArray readStream)) hex ].
	hash := Smalltalk at: #CMD5Hasher ifPresent: [ :cls |
		cls hashMessage: aString ].
	hash ifNil: [
		hash := Smalltalk at: #TCryptoRandom ifPresent: [ :cls |
			(cls basicNew perform: #md5HashMessage: with: aString) asInteger ] ].
	hash ifNotNil: [
		hash := hash hex asLowercase.
		(hash beginsWith: '16r') ifTrue: [ hash := hash allButFirst: 3 ].
		hash := hash padLeftTo: 32 with: $0 ].
	^ hash
