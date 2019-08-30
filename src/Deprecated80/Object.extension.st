Extension { #name : #Object }

{ #category : #'*Deprecated80' }
Object >> asSetElement [
	self deprecated: 'Use #asCollectionElement instead' transformWith: '`@receiver asSetElement' -> '`@receiver asCollectionElement'.
	^ self asCollectionElement
]

{ #category : #'*Deprecated80' }
Object >> crLog [
	self
		deprecated: 'Please use #crTrace instead'
		transformWith: '`@receiver crLog' -> '`@receiver crTrace'.
	self crTrace
]

{ #category : #'*Deprecated80' }
Object >> crLog: aString [
	"Log the argument. Use self log: instead of Transcript cr; show: "

	self
		deprecated: 'Please use trace* methods instead.'
		transformWith:
			'`@receiver crLog: `@statements1'
				-> '`@receiver crTrace: `@statements1'.
	self crTrace: aString
]

{ #category : #'*Deprecated80' }
Object >> enclosedSetElement [
	self deprecated: 'Use #enclosedElement instead' transformWith: '`@receiver enclosedSetElement' -> '`@receiver enclosedElement'.
	^ self enclosedElement
]

{ #category : #'*Deprecated80' }
Object >> log: aString [
	"Log the argument. Use self log: instead of Transcript show: "
	
	self
		deprecated: 'Please use trace* methods instead.'
		transformWith: '`@receiver log: `@statements1' 
						-> '`@receiver trace: `@statements1'.
	
	self trace: aString
]

{ #category : #'*Deprecated80' }
Object >> logCr [
	self
		deprecated: 'Please use trace* methods instead.'
		transformWith: '`@receiver logCr' -> '`@receiver traceCr'.
	self traceCr
]

{ #category : #'*Deprecated80' }
Object >> logCr: aString [
	"Log the argument. Use self logCr: 'something' instead of Transcript show: 'something'  ; cr "

	self
		deprecated: 'Please use trace* methods instead.'
		transformWith:
			'`@receiver logCr: `@statements1'
				-> '`@receiver traceCr: `@statements1'.
	self traceCr: aString
]

{ #category : #'*Deprecated80' }
Object >> logCrTab: aString [
	"Log the argument. Use self logCr: 'something' instead of Transcript show: 'something'  ; cr ; tab"
	
	self
		deprecated: 'Please use trace* methods instead.'
		transformWith: '`@receiver logCrTab: `@statements1' 
						-> '`@receiver traceCrTab: `@statements1'.
	
	self traceCrTab: aString
]
