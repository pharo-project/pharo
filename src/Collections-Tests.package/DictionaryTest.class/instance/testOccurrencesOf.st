tests - testing
testOccurrencesOf
	"self run:#testOccurrencesOf" 
	
	| dict |
	dict := self collectionClass new.
	dict at: #a put: 1.
	dict at: #b put: 2.
	dict at: #c put: 1.
	dict at: #d put: 3.
	dict at: nil put: nil.
	dict at: #z put: nil.
	
	
	self assert: (dict occurrencesOf: 1 ) equals: 2.
	self supportsNilKey 
		ifTrue: [ 	self assert: (dict occurrencesOf: nil ) equals: 2 ]
		ifFalse: [ 	self assert: (dict occurrencesOf: nil ) equals: 1 ].	
	
	
	