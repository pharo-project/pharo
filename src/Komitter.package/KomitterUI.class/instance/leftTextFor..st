private
leftTextFor: aKOObject

	aKOObject ifNil: [ ^ '' ].
	^ aKOObject koSourceText