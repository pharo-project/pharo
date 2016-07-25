accessing
allPoolDictionaryNames
	| sprClass |
	sprClass := self superclass.
	^sprClass isNil 
		ifTrue: [self sharedPoolNames]
		ifFalse: [sprClass allPoolDictionaryNames , self sharedPoolNames]