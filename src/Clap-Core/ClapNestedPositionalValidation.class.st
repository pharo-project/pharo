"
I validate that a nested positional, i.e. a positional preceded by a flag is correct:
- there is no positional found in the command line but a default value was specified,
- or there is a positional found after the flag.
"
Class {
	#name : 'ClapNestedPositionalValidation',
	#superclass : 'ClapValidation',
	#instVars : [
		'missing'
	],
	#category : 'Clap-Core-Validation',
	#package : 'Clap-Core',
	#tag : 'Validation'
}

{ #category : 'accessing' }
ClapNestedPositionalValidation >> description [
	^ 'Missing positional value for ', (', ' join: self missingValueIdentifiers)
]

{ #category : 'validation' }
ClapNestedPositionalValidation >> failureDescriptionFor: anObject [
	^ self description
]

{ #category : 'initialization' }
ClapNestedPositionalValidation >> initialize [ 

	missing := OrderedCollection new.
]

{ #category : 'validation' }
ClapNestedPositionalValidation >> isValidNamedMatch: aClapNamedMatch [

	| isValid |
	isValid := aClapNamedMatch isFullMatch.
	isValid ifFalse: [ missing add: aClapNamedMatch ].
	^ isValid
]

{ #category : 'accessing' }
ClapNestedPositionalValidation >> missingValueIdentifiers [

	^ missing flatCollect: [ :match | 
		  match specification positionals collect: [ :positional | positional identifier ] ]
]
