private
basicHandle: aHandle at: index
	| data |
	data := ExternalData 
		fromHandle: (aHandle pointerAt: index) 
		type: ExternalType string.
	data isNull ifTrue: [ ^ nil ].
	^ data fromCString