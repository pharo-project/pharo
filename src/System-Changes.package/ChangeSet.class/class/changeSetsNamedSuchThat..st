enumerating
changeSetsNamedSuchThat: nameBlock
	"(ChangeSet changeSetsNamedSuchThat:
		[:name | name first isDigit and: [name initialInteger >= 373]])
		do: [:cs | AllChangeSets remove: cs wither]"

	^ AllChangeSets select: [:aChangeSet | nameBlock value: aChangeSet name]