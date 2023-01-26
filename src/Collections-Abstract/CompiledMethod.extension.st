Extension { #name : #CompiledMethod }

{ #category : #'*Collections-Abstract' }
CompiledMethod class >> sortBlock [
	"Return a sort block that orders methods by class name and then by selector"
	^  [ :a :b|
			a methodClass = b methodClass
				ifTrue: [ a selector <= b selector ]
				ifFalse: [ a methodClass name <= b methodClass name ]]
]
