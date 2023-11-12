Extension { #name : 'RPackageOrganizer' }

{ #category : '*InitializePackagesCommandLineHandler' }
RPackageOrganizer >> basicBootstrapInitialize [

	| allBehaviors |
	allBehaviors := Smalltalk allClassesAndTraits.

	"This is a hack because of a weird behavior produced by the bootstrap were the package or tags end up containing classes that are not the same instances than we have in the system dictionary..To be investigated..."
	self packages do: [ :package |
		package classTags do: [ :tag |
			tag instVarNamed: #classes put: (tag classes collect: [ :class | allBehaviors detect: [ :aClass | aClass name = class name ] ]) ] ]
]
