printing
configMethodCascadeOn: aStream indent: indent

	| repositorySpecs |
	repositorySpecs := self map values sort: [:a :b | a description <= b description ].
	repositorySpecs size = 1
		ifTrue: [ repositorySpecs first configMethodCascadeOn: aStream lastCascade: true ]
		ifFalse: [
			1 to: repositorySpecs size do: [:index | | repositorySpec |
				aStream tab: indent + 1.
				(repositorySpecs at: index)
					configMethodCascadeOn: aStream 
					lastCascade: index >= repositorySpecs size ]]