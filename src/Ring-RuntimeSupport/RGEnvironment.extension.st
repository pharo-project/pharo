Extension { #name : #RGEnvironment }

{ #category : #'*Ring-RuntimeSupport' }
RGEnvironment >> addGlobalsNamed: anArrayOfSymbols [

	anArrayOfSymbols do: [ :each |
		self addGlobalVariable: (RGGlobalVariable named: each parent: self)].
]

{ #category : #'*Ring-RuntimeSupport' }
RGEnvironment >> hasBindingThatBeginsWith: aString [
	"Use the cached class and non-class names for better performance."

	| aName searchBlock |
	searchBlock := [ :element |
		(element beginsWith: aString)
			ifTrue: [ 0 ]
			ifFalse: [
				aString < element
					ifTrue: [ -1 ]
					ifFalse: [ 1 ] ] ].
	aName := self classNames 
		findBinary: searchBlock
		ifNone: [nil].
	aName ifNotNil: [ ^true ].
	aName := self nonClassNames 
		findBinary: searchBlock
		ifNone: [nil].
	^aName notNil
]
