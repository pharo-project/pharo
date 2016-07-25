queries
classes
	| classesCollection |
	classesCollection := OrderedCollection new.
	methods
		do: [ :method | method methodClass ifNotNil: [ classesCollection addIfNotPresent:  method methodClass theNonMetaClass ] ].
	^ classesCollection