format
formatOfClass: aRFiClass 
	
	"A class format is composed by"
	"<2 bits of size><5 bits of compact class index><4 bits of inst spec><6 bits of size><1 bit with a 0>"
	
	| size1 instSpec size2 compactClassIndex |
	size1 := (aRFiClass allInstVarNames size + 1 // 64) bitAnd: 16r3.
	instSpec := aRFiClass isCompiledMethod
					ifTrue: [ 12 ]
					ifFalse: [aRFiClass isWeak
						ifTrue:[ 4 ]
						ifFalse:[aRFiClass isPointers
									ifTrue: [ aRFiClass isVariable
												ifTrue: [ aRFiClass allInstVarNames size > 0 ifTrue: [ 3 ] ifFalse: [ 2 ] ]
												ifFalse: [ aRFiClass allInstVarNames size > 0 ifTrue: [ 1 ] ifFalse: [ 0 ] ]
											]
									ifFalse: [ aRFiClass isWords ifTrue: [ 6 ] ifFalse: [ 8 ] ]
								]
					].
	size2 := (aRFiClass allInstVarNames size + 1 \\ 64) bitAnd: 16r3F.
	
	compactClassIndex := self compactClassIndexForClass: aRFiClass.
	
	^(size1 bitShift: 16) + (compactClassIndex bitShift: 11) + (instSpec bitShift: 7) + (size2 bitShift: 1)