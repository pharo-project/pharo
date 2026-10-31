Extension { #name : 'Array' }

{ #category : '*DeepCopier' }
Array >> copyWithDependent: newElement [

	self isEmpty ifTrue: [ ^ DependentsArray with: newElement ].
	^ self copyWith: newElement
]
