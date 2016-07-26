private
packageSatisfying: aBlock
	"Answer the first package satisfying aBlock or nil. This method assumes that it is likely that the last matching package matches the given condition again and thus it tries that one first."

	(package notNil and: [ aBlock value: package ])
		ifTrue: [ ^ package ].
	packages do: [ :info |
		(aBlock value: info)
			ifTrue: [ ^ package := info ] ].
	^ nil