Extension { #name : 'MetacelloProjectRegistration' }

{ #category : '*Deprecated14' }
MetacelloProjectRegistration >> baselineProjectSpec [

	self deprecated: 'Use #projectSpec instead.' transformWith: '`@rcv baselineProjectSpec' -> '`@rcv projectSpec'.
	^ self projectSpec
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistration class >> baselineProjectSpecs [

	self deprecated: 'Use #projects on the Registry instead.' transformWith: '`@rcv baselineProjectSpecs' -> 'MetacelloProjectRegistry projects'.
	^ MetacelloProjectRegistry projects
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistration class >> projectSpecs [

	self deprecated: 'Use #projects on the Registry instead.' transformWith: '`@rcv projectSpecs' -> 'MetacelloProjectRegistry projects'.
	^ MetacelloProjectRegistry projects
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistration class >> registrationForProjectSpec: aProjectSpec ifPresent: presentBlock [

	self
		deprecated:
		'The registry should be accessed via MetacelloProjectRegistry and not MetacelloProjectRegistration. Also, to retrieve the registration you should now gite the name of the project instead of a project spec'
		transformWith:
		'`@rcv registrationForProjectSpec: `@arg1 ifPresent: `@arg2' -> 'MetacelloProjectRegistry registrationNamed: `@arg1 className ifPresent: `@arg2'.
	^ MetacelloProjectRegistry registrationNamed: aProjectSpec className ifPresent: presentBlock
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistration class >> registrationForProjectSpec: aProjectSpec ifPresent: presentBlock ifAbsent: absentBlock [

	self
		deprecated:
		'The registry should be accessed via MetacelloProjectRegistry and not MetacelloProjectRegistration. Also, to retrieve the registration you should now gite the name of the project instead of a project spec'
		transformWith: '`@rcv registrationForProjectSpec: `@arg1 ifPresent: `@arg2 ifAbsent: `@arg3'
			-> 'MetacelloProjectRegistry registrationNamed: `@arg1 className ifPresent: `@arg2 ifAbsent: `@arg3'.
	^ MetacelloProjectRegistry registrationNamed: aProjectSpec className ifPresent: presentBlock ifAbsent: absentBlock
]

{ #category : '*Deprecated14' }
MetacelloProjectRegistration class >> registry: aMetacelloProjectRegistry [

	self
		deprecated: 'The registry should be accessed via MetacelloProjectRegistry and not MetacelloProjectRegistration.'
		transformWith: '`@rcv registry: `@arg' -> 'MetacelloProjectRegistry registry: `@arg'.
	MetacelloProjectRegistry registry: aMetacelloProjectRegistry
]
