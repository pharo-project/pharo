Class {
	#name : #ClapFlagTest,
	#superclass : #ClapParameterizedTest,
	#category : #'Clap-Tests-Unit'
}

{ #category : #accessing }
ClapFlagTest >> classToTest [
	^ ClapFlag
]

{ #category : #tests }
ClapFlagTest >> testMatchesLongForm [
	| argv flagName match |
	flagName := '--' , self subjectName.
	argv := { flagName. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: '--foo'.
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapFlagTest >> testMatchesShortForm [
	| argv flagName match |
	flagName := '-' , (self subjectName first: 1).
	argv := { flagName. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: flagName.
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapFlagTest >> testMismatchesWrongLongForm [
	| argv badFlag match |
	badFlag := '--' , self subjectName , 'NOT'.
	argv := { badFlag } readStream.
	
	match := subject matchOn: argv.
	
	self assert: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: badFlag.
	self assert: argv next identicalTo: badFlag
]

{ #category : #tests }
ClapFlagTest >> testMismatchesWrongShortForm [
	| argv badFlag match |
	badFlag := '-' , (self subjectName first: 1) asUppercase.
	argv := { badFlag } readStream.
	
	match := subject matchOn: argv.
	
	self assert: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: badFlag.
	self assert: argv next identicalTo: badFlag
]
