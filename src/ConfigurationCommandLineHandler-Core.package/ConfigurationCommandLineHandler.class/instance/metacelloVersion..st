actions
metacelloVersion: aVersionName
	| project |
	project := self project.
	^ [ project version: aVersionName ] 
		on: MetacelloVersionDoesNotExistError do: [ :error |
			aVersionName = 'last'
				ifTrue: [
					"manual fallback since there is no symbolic name for lastVersion"
					project lastVersion ]
				ifFalse: [ 
					"symbols and strings are not equal in Meteacello..."
					project version: aVersionName asSymbol ]].