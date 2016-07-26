private
extractNameFromFile

	file == nil ifTrue: [ ^nil ].
	^(self loader nameComponentsFrom: self file) first