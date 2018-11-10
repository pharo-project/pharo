Class {
	#name : #TonelScannerTest,
	#superclass : #MCTestCase,
	#category : #'MonticelloTonel-Tests'
}

{ #category : #tests }
TonelScannerTest >> testRemoveFromEnclosingStartEnd [
	| scanner nl |
	
	nl := String cr.
	scanner := TonelScanner basicNew.
	self 
		assert: (scanner 
			removeFrom: '[ ^ self ]'  
			enclosingStart: $[ 
			end: $])
		equals: '^ self'.
		
	self 
		assert: (scanner 
			removeFrom: ('[ 
	^ self ]' withLineEndings: nl) 
			enclosingStart: $[ 
			end: $])
		equals: Character tab asString, '^ self'.
	
	self 
		assert: (scanner 
			removeFrom: ('[ 
	^ self
	]' withLineEndings: nl)
			enclosingStart: $[ 
			end: $])
		equals: Character tab asString, '^ self'.
		
	self 
		assert: (scanner 
			removeFrom: ('[ 

	^ self

]' withLineEndings: nl)
			enclosingStart: $[ 
			end: $])
		equals: (('
	^ self
') withLineEndings: nl)
]
