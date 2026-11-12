Class {
	#name : 'ClapFlagTest',
	#superclass : 'ClapParameterizedTest',
	#category : 'Clap-Tests-Unit',
	#package : 'Clap-Tests',
	#tag : 'Unit'
}

{ #category : 'accessing' }
ClapFlagTest >> classToTest [
	^ ClapFlag
]

{ #category : 'tests' }
ClapFlagTest >> commandWithOptionalPositional [
	"A command whose repeatable flag --dev takes a name, a repository and optional groups"

	^ (ClapCommandSpec id: #install)
		  add: ((ClapFlag id: #dev)
				   add: (ClapPositional id: #name);
				   add: (ClapPositional id: #repository);
				   add: ((ClapPositional id: #groups)
						    beOptional;
						    defaultValue: [ 'default' ];
						    yourself);
				   multiple: true;
				   yourself);
		  addFlag: #quiet description: 'Say nothing';
		  yourself
]

{ #category : 'tests' }
ClapFlagTest >> devsIn: anArray [

	^ ((self commandWithOptionalPositional match: anArray) occurrencesOf: #dev collect: [ :each |
			   {
				   (each at: #name) value.
				   (each at: #repository) value.
				   (each at: #groups) value } ]) asArray
]

{ #category : 'tests' }
ClapFlagTest >> testAMandatoryPositionalCannotBeLeftOut [

	self deny: (self commandWithOptionalPositional match: #( 'install' '--dev' 'Bloc' )) isValid
]

{ #category : 'tests' }
ClapFlagTest >> testAnOptionalPositionalCanBeGiven [

	self assert: (self devsIn: #( 'install' '--dev' 'Bloc' 'repo' 'Core,Tests' )) equals: #( #( 'Bloc' 'repo' 'Core,Tests' ) )
]

{ #category : 'tests' }
ClapFlagTest >> testAnOptionalPositionalCanBeLeftOut [
	"Left out, it takes its default value, even when another flag follows"

	self
		assert: (self devsIn: #( 'install' '--dev' 'Bloc' 'repo' '--dev' 'Album' 'other' 'Tests' '--quiet' ))
		equals: #( #( 'Bloc' 'repo' 'default' ) #( 'Album' 'other' 'Tests' ) ).
	self assert: (self commandWithOptionalPositional match: #( 'install' '--dev' 'Bloc' 'repo' '--quiet' )) isValid
]

{ #category : 'tests' }
ClapFlagTest >> testAnOptionalPositionalIsBracketedInTheSynopsis [

	self
		assert: (String streamContents: [ :stream |
					 (ClapPositional id: #groups)
						 beOptional;
						 synopsisOn: stream ])
		equals: '[<groups>]'
]

{ #category : 'tests' }
ClapFlagTest >> testMatchesLongForm [
	| subject argv flagName match |
	subject := self namedSubject.
	flagName := '--' , self subjectName.
	argv := { flagName. #remainder } readStream.
	
	match := subject matchOn: argv in: ClapContext new.
	
	self deny: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: '--foo'.
	self assert: argv next equals: #remainder
]

{ #category : 'tests' }
ClapFlagTest >> testMatchesShortForm [
	| subject argv flagName match |
	subject := self namedSubject.
	flagName := '-' , (self subjectName first: 1).
	argv := { flagName. #remainder } readStream.
	
	match := subject matchOn: argv in: ClapContext new.
	
	self deny: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: flagName.
	self assert: argv next equals: #remainder
]

{ #category : 'tests' }
ClapFlagTest >> testMismatchesWrongLongForm [
	| subject argv badFlag match |
	subject := self namedSubject.
	badFlag := '--' , self subjectName , 'NOT'.
	argv := { badFlag } readStream.
	
	match := subject matchOn: argv in: Object new.
	
	self assert: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: badFlag.
	self assert: argv next identicalTo: badFlag
]

{ #category : 'tests' }
ClapFlagTest >> testMismatchesWrongShortForm [
	| subject argv badFlag match |
	subject := self namedSubject.
	badFlag := '-' , (self subjectName first: 1) asUppercase.
	argv := { badFlag } readStream.
	
	match := subject matchOn: argv in: Object new.
	
	self assert: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: badFlag.
	self assert: argv next identicalTo: badFlag
]
