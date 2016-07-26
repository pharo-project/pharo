query
obsoleteMethodReferences
	"SystemNavigation new obsoleteMethodReferences"

	"Open a browser on all referenced behaviors that are obsolete"

	| obsClasses references |
	references := Array new writeStream.
	obsClasses := self obsoleteBehaviors.
	'Scanning for methods referencing obsolete classes' 
		displayProgressFrom: 0 to: obsClasses size
		during: 
			[ :bar | 
			obsClasses keysAndValuesDo: 
					[ :index :each | | obsRefs | 
					bar current: index.
					obsRefs := each pointersToExcept: obsClasses.
					obsRefs do: 
							[ :ref | 
							"Figure out if it may be a global"
							(ref isVariableBinding and: [ref key isString	"or Symbol"]) 
								ifTrue: 
									[(ref pointersTo) do: 
											[:meth | 
											meth isCompiledMethod
												ifTrue: [meth methodReference ifNotNil: [:mref | references nextPut: mref]]]]]]].
	^ references contents