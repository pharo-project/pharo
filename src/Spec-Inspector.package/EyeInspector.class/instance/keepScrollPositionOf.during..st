event-handling
keepScrollPositionOf: specHolder during: aBlock
	| scrollValue |
	specHolder widget 
		ifNil: [ ^ aBlock value ].
		
	scrollValue := description scrollValue.
	aBlock value.
	description scrollValue: scrollValue.