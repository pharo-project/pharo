private
collapseZeros
	"the rule must be that zeros can be collapsed as long as the series of zeros ends in a string term"

	| collection newSize new j lastElementIsStringOrZero canCollapse |
	(self size = 0 or: [ self at: 1 ]) == 0
		ifTrue: [ ^ self ].
	collection := OrderedCollection new.
	lastElementIsStringOrZero := true.
	canCollapse := true.
	self size to: 1 by: -1 do: [ :i | 
		| element |
		element := self at: i.
		(canCollapse and: [ element == 0 ])
			ifTrue: [ 
				lastElementIsStringOrZero
					ifFalse: [ 
						canCollapse := false.
						collection addFirst: element.]]
			ifFalse: [ 
				collection addFirst: element.
				canCollapse := lastElementIsStringOrZero := element isString ] ].
	collection size = self size
		ifTrue: [ ^ self ].
	newSize := collection size.
	new := self species new: newSize.
	j := 0.
	collection
		do: [ :element | 
			new at: j + 1 put: element.
			j := j + 1 ].
	^ new