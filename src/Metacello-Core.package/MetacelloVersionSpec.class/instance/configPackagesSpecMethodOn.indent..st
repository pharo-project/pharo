printing
configPackagesSpecMethodOn: aStream indent: indent

	| projectSpecs packageSpecs groupSpecs |
	projectSpecs := OrderedCollection new.
	packageSpecs := OrderedCollection new.
	groupSpecs := OrderedCollection new.
	self packagesSpec list do: [:member |
		member spec 
			projectDo: [:proj | member spec name ~~ nil ifTrue: [ projectSpecs add: member ]]
			packageDo: [:package | member spec name ~~ nil ifTrue: [ packageSpecs add: member ]]
			groupDo: [:group | member spec name ~~ nil ifTrue: [ groupSpecs add: member ]]].
	projectSpecs isEmpty not
		ifTrue: [
			aStream tab: indent; nextPutAll: 'spec '.
			projectSpecs size > 1 ifTrue: [ aStream cr; tab: indent + 1 ].
			1 to: projectSpecs size  do: [:index | 
				(projectSpecs at: index) configMethodCascadeOn: aStream last: index == projectSpecs size indent: indent + 1.
				index ~= projectSpecs size ifTrue: [ aStream tab: indent + 1 ]]].
	packageSpecs isEmpty not
		ifTrue: [
			projectSpecs isEmpty not ifTrue: [ aStream cr ].
			aStream tab: indent; nextPutAll: 'spec '.
			packageSpecs size > 1 ifTrue: [ aStream cr; tab: indent + 1 ].
			1 to: packageSpecs size  do: [:index | 
				(packageSpecs at: index) configMethodCascadeOn: aStream last: index == packageSpecs size indent: indent + 1.
				index ~= packageSpecs size ifTrue: [ aStream tab: indent + 1 ]]].
	groupSpecs isEmpty not
		ifTrue: [
			projectSpecs isEmpty not | packageSpecs isEmpty not ifTrue: [ aStream cr ].
			aStream tab: indent; nextPutAll: 'spec '.
			groupSpecs size > 1 ifTrue: [ aStream cr; tab: indent + 1 ].
			1 to: groupSpecs size  do: [:index | 
				(groupSpecs at: index) configMethodCascadeOn: aStream last: index == groupSpecs size indent: indent + 1.
				index ~= groupSpecs size ifTrue: [ aStream tab: indent + 1 ]]].