api
definesSlotNamed: aString 
	^self resolveSlot: aString ifFound: [ true ] ifNone: [ false ].