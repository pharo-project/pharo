private
detachResultFromSystem
	result ifNil: [ ^ self ].
	result detachFromSystem.
	result := nil.