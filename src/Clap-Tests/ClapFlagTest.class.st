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
