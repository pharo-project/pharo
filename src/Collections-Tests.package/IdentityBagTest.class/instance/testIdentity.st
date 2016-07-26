tests
testIdentity
	"self run:#testIdentity" 
	"self debug:#testIdentity"
	| bag identityBag aString anOtherString |
	
	aString := 'hello'.
	anOtherString := aString copy.
	
	self assert: (aString = anOtherString).
	self assert: (aString == anOtherString) not.

	bag := Bag new.
	bag add: aString.
	bag add: aString.
	bag add: anOtherString.
	self assert: (bag occurrencesOf: aString) = 3.
	self assert: (bag occurrencesOf: anOtherString) = 3.
	
	identityBag := IdentityBag new.
	identityBag add: aString.
	identityBag add: aString.
	identityBag add: anOtherString.
	
	self assert: (identityBag occurrencesOf: aString) = 2.
	self assert: (identityBag occurrencesOf: anOtherString) = 1.



