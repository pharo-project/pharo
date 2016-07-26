accessing
itemsForDirectory: aFileDirectory
	"Answer a list of services appropriate when no file is selected."

	| services |
	services := OrderedCollection new.
	(PragmaCollector filter: [ :pragma | pragma keyword = 'directoryService' ]) reset
		do: [ :each | services addAll: (each methodClass soleInstance perform: each selector with: aFileDirectory) ].
	^ services