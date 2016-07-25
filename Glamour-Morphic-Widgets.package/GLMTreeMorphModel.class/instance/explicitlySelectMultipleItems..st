private selecting
explicitlySelectMultipleItems: aCollection 
	| newSelection collection|
	aCollection isNil ifTrue: [^ self].
	collection := aCollection isCollection ifTrue: [aCollection] ifFalse: [aCollection asOrderedCollection ].
	(self selection notNil and: [
		self selection selectedItemOrItemsOrNil = collection asOrderedCollection ]) ifTrue: [ ^ self ].
	newSelection := OrderedCollection new.
	self roots do: [:eachRoot |
		eachRoot withContentsDo: [:each |
			(collection includes: each item) ifTrue: [
				newSelection add: each path ]]].
	self selectAllNodePaths: newSelection