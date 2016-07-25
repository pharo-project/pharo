interpreting
interpretASpec: aPresenter selector: aSelector
	aPresenter ifNil: [ ^ nil ].
	
	(self extractArrayToInterpretFrom: aPresenter) ifNotNil: [ :result | ^ result ].
	
	(self retrieveSpecFrom: aPresenter selector: aSelector)
		ifNotNil: [ :instance | ^ instance ].

	index := 2.
	[ index <= arrayToInterpret size ]
		whileTrue: [ 
			| newInstance |
			
			newInstance := self performNextSelectorAndIncrementIndex.
			
			index > arrayToInterpret size
				ifTrue: [ ^ self returnInterpretationOf: newInstance ].
			spec instance: newInstance ].	
			
	"If you get here, there is a problem. Must probably it comes from the Spec which is wrongly defined"
	self assert: arrayToInterpret size < 2.
	
	(model respondsTo: #spec:)
		ifTrue: [ model spec: spec ].
		
	^ spec instance