accessing
itemsForFile: fullName
	"Answer a list of services appropriate for a file of the given full name"

	| services suffix |
	suffix := self suffixOf: fullName.
	services := OrderedCollection new.
	(PragmaCollector filter: [ :pragma | pragma keyword = 'fileService' ])
		reset
		do: [ :each | 
			services
				addAll:
					(each methodClass soleInstance
						perform: each methodSelector
						withArguments:
							{fullName.
							suffix}) ].
	^ services