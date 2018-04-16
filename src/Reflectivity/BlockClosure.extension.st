Extension { #name : #BlockClosure }

{ #category : #'*Reflectivity' }
BlockClosure >> rfEnsure: aBlock [
	"same as #esure, carefully written to never have active meta-links as it is called in the code path that checks for recursion"

	<primitive: 198>
	<metaLinkOptions: #( + optionDisabledLink)>
	| returnValue b |
	returnValue := self rfvalue.
	aBlock isNil
		ifFalse: [ 
			"nil out aBlock temp before evaluating aBlock so it is not executed again if aBlock remote returns"
			b := aBlock.
			thisContext rftempAt: 1 put: nil.
			b rfvalue ].
	^ returnValue
]

{ #category : #'*Reflectivity' }
BlockClosure >> rfvalue [
	"same as value, for recursion stopping metalinks"
	<primitive: 201>
	<metaLinkOptions: #( + optionDisabledLink)>
	^self primitiveFailed
]

{ #category : #'*Reflectivity' }
BlockClosure >> valueSelector [
	self numArgs = 0 ifTrue: [^#value].
	^(String streamContents: [:stream | 
		stream nextPutAll: 'value:'.
		(self numArgs - 1) timesRepeat: [stream nextPutAll: 'value:']]) asSymbol.
]
