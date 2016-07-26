private
execute: anOperationClass do: aBlock
	| operation |
	operation := anOperationClass on: self copy.
	aBlock ifNotNil: [ aBlock value: operation ].
	^ operation execute