filein/out
fileOutChangeSetsNamed: nameList
	"File out the list of change sets whose names are provided"
     "ChangesOrganizer fileOutChangeSetsNamed: #('New Changes' 'miscTidies-sw')"

	| notFound empty infoString |
	notFound := OrderedCollection new.
	empty := OrderedCollection new.
	nameList do:
		[:aName | | aChangeSet | 
			(aChangeSet := self named: aName)
				ifNotNil: [aChangeSet isEmpty
							ifTrue: [empty add: aName]
							ifFalse: [aChangeSet fileOut]]
				ifNil: [notFound add: aName]].

	infoString := (nameList size - notFound size) printString, ' change set(s) filed out'.
	notFound notEmpty ifTrue:
		[infoString := infoString, '

', notFound size printString, ' change set(s) not found:'.
		notFound do:
			[:aName | infoString := infoString, '
', aName]].
	empty notEmpty ifTrue:
		[infoString := infoString, '
', empty size printString, ' change set(s) were empty:'.
		empty do:
			[:aName | infoString := infoString, '
', aName]].

	self inform: infoString