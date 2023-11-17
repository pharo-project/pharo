Extension { #name : 'PackageOrganizer' }

{ #category : '*InitializePackagesCommandLineHandler' }
PackageOrganizer >> basicBootstrapInitialize [

	| allBehaviors |
	allBehaviors := Smalltalk allClassesAndTraits.

	"This is a hack because of a weird behavior produced by the bootstrap were the package or tags end up containing classes that are not the same instances than we have in the system dictionary..To be investigated..."
	self packages do: [ :package |
		package classTags do: [ :tag |
			tag instVarNamed: #classes put: (tag classes collect: [ :class | allBehaviors detect: [ :aClass | aClass name = class name ] ]) ] ].

	self flag: #todo. "This next step should not be needed when we will not use announcements to manage method addition"
	allBehaviors
		do: [ :behavior |
			{
				behavior.
				behavior classSide } do: [ :aBehavior |
				aBehavior extensionProtocols do: [ :protocol |
					(self ensurePackageMatching: protocol name allButFirst trimBoth) importProtocol: protocol forClass: aBehavior ] ] ]
		displayingProgress: 'Importing extensions'
]
