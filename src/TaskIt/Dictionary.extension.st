Extension { #name : #Dictionary }

{ #category : #'*TaskIt' }
Dictionary >> removeKeyAtValue: aValue [
	| key |
	key := self keyAtValue: aValue ifAbsent: [ nil ].
	key ifNotNil: [ self removeKey: key ].
	^ key
]
