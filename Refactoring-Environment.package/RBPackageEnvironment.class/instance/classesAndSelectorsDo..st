accessing
classesAndSelectorsDo: aBlock
	packages do: [ :package |
		package methods do: [ :method |
			(environment includesSelector: method selector in: method methodClass)
				ifTrue: [ aBlock value: method methodClass value: method selector ] ] ]