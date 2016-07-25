updating
update: anArrayOrSymbol
	"A package has been modified (loaded, removed, or updated). Find out if any configurations are managing the package.
	Update events are coming from MCWorkingCopy."

	| packageName operation |
	anArrayOrSymbol isArray
		ifFalse: [ ^ self ].
	operation := anArrayOrSymbol at: 1.
	operation == #modified
		ifTrue: [ packageName := (anArrayOrSymbol at: 2) packageName ].
	operation == #registered
		ifTrue: [ packageName := (anArrayOrSymbol at: 2) name ].
	operation == #unregistered
		ifTrue: [ packageName := (anArrayOrSymbol at: 2) name ].
	(packageName beginsWith: 'ConfigurationOf')
		ifTrue: [ 
			| wc info |
			operation ~~ #modified
				ifTrue: [ ^ self ].	"only interested in modified"
			wc := anArrayOrSymbol at: 2.
			wc modified
				ifTrue: [ ^ self ].	"only interested in transtion from modified to not modified"
			(info := self configurationInfoFor: (Smalltalk at: packageName asSymbol ifAbsent: [ ^ self ])) isNil
				ifTrue: [ ^ self ].	
			"removed and added packages handled by the add/removal of the class"
			info configurationClassModified.
			^ self ].
	self configurations do: [ :info | info recalculateIfInterestedInPackageNamed: packageName for: operation ]