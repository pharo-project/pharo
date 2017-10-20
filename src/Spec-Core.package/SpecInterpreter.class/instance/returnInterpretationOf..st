interpreting-private
returnInterpretationOf: newInstance
	| result |
	(model respondsTo: #spec:)
		ifTrue: [ model spec: spec ].
	result := self class private_interpretASpec: newInstance model: spec instance selector: spec selector.
	^ (result isKindOf: ComposablePresenter)
		ifTrue: [ 
			| return |
			return := result private_buildWithSpec.
			^ return ]
		ifFalse: [ ^ result ]