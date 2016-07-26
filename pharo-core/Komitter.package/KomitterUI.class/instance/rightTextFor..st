private
rightTextFor: aKOObject

	aKOObject ifNil: [ ^ '' ].
	^ aKOObject koDestinationText