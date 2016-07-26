accessing
classesDo: aBlock
	| enumerated enumerator |
	enumerated := IdentitySet new.
	enumerator := [ :each |
		(enumerated includes: each) ifFalse: [
			(environment includesClass: each)
				ifTrue: [ aBlock value: each ].
			(environment includesClass: each theMetaClass)
				ifTrue: [ aBlock value: each theMetaClass].
			enumerated add: each ] ].
	packages do: [ :package |
		package classes do: enumerator.
		package extensionClasses do: enumerator ]