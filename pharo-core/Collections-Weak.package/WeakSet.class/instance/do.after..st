public
do: aBlock after: anElement

	| startIndex |
	tally = 0 ifTrue: [ ^self ].
	startIndex := anElement
		ifNil: [ 0 ]
		ifNotNil: [ self scanFor: anElement ].
	startIndex + 1 to: array size do: [ :index |
		(array at: index) ifNotNil: [ :object |
			object == flag ifFalse: [
				aBlock value: object enclosedSetElement] ] ]