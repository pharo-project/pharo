private
computePathFor: anObject 
	| path object |
	path := OrderedCollection new.
	object := anObject.
	[object isNil] whileFalse: 
			[path add: object.
			object := self backlinkFor: object].
	^path reverse