private
classSearch: aSelectBlock
	| result |
	result := OrderedCollection new.
	self packagesSelection classesAndTraitsDo:[ :class | 
			(aSelectBlock value: class)
				ifTrue: [ result add: class ] ].
	^ result