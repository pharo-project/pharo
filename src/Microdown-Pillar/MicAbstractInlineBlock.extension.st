Extension { #name : #MicAbstractInlineBlock }

{ #category : #'*Microdown-Pillar' }
MicAbstractInlineBlock >> asPillar [

	| childrenAsPillar |
	childrenAsPillar := self isOnlyChild 
		ifTrue: [ {(PRText new text: self substring )} ] 
		ifFalse: [ children collect: [:e | e asPillar ] ].
	 ^ self associatedPillarClass new
				setChildren: childrenAsPillar;
				yourself
]

{ #category : #'*Microdown-Pillar' }
MicAbstractInlineBlock >> associatedPillarClass [
	^ self subclassResponsibility
]
