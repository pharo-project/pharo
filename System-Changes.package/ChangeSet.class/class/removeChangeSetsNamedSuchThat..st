enumerating
removeChangeSetsNamedSuchThat: nameBlock
	(self changeSetsNamedSuchThat: nameBlock)
		do: [:cs | self removeChangeSet: cs]