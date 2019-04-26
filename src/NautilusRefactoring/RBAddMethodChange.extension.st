Extension { #name : #RBAddMethodChange }

{ #category : #'*NautilusRefactoring' }
RBAddMethodChange >> accept: aText notifying: aController [
	"Just to make sure that it compiles, try with the standard compiler."
	
	| compiler |
	compiler := self changeClass
		ifNil: [ Object compiler ]
		ifNotNil: [:changeClass | changeClass compiler ].
		
	compiler
		source: aText asString;
		class:  self changeClass;
		requestor: aController;
		failBlock: [ ^ false ];
		compile.
	
		
	self 
		class: self changeClass
		protocol: self protocol
		source: aText asString.
		
	^ true
]
