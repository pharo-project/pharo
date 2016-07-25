private
removeEverythingInSetFromSystem: aChangeSet 

	aChangeSet changedMessageList
		do: [:methodRef | methodRef actualClass removeSelector: methodRef selector].
	aChangeSet changedClasses
		do: [:each | each isMeta
				ifFalse: [each removeFromSystemUnlogged]]