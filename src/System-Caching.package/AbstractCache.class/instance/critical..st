private
critical: block
	^ access
		ifNil: block
		ifNotNil: [ access critical: block ]