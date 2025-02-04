Extension { #name : 'Array' }

{ #category : '*System-Model' }
Array >> copyWithDependent: newElement [

	self isEmpty ifTrue: [ ^ DependentsArray with: newElement ].
	^ self copyWith: newElement
]
