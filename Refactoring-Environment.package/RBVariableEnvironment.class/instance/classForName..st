private
classForName: aString 
	| name isMeta class |
	isMeta := aString includes: $ .
	name := (isMeta 
		ifTrue: [ aString copyFrom: 1 to: (aString size - 6 max: 1) ]
		ifFalse: [ aString ]) asSymbol.
	class := self systemDictionary at: name ifAbsent: [ nil ].
	^ (class notNil and: [ isMeta ])
		ifTrue: [ class class ]
		ifFalse: [ class ]