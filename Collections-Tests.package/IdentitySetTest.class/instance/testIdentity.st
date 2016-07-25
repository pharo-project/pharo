tests - identity
testIdentity
	"self run:#testIdentity" 
	"self debug:#testIdentity"
	| identitySet aString anOtherString |
	
	aString := 'hello'.
	anOtherString := aString copy.
	
	self assert: (aString = anOtherString).
	self assert: (aString == anOtherString) not.

	
	identitySet := self classToBeTested  new.
	identitySet add: aString.

	
	self assert: (identitySet occurrencesOf: aString) = 1.
	self assert: (identitySet occurrencesOf: anOtherString) = 0.
	

	self assert: (identitySet includes: aString).
	self deny: (identitySet includes: anOtherString) = 0.