Class {
	#name : #ClapCommandTest,
	#superclass : #ClapParameterizedTest,
	#category : #'Clap-Tests-Unit'
}

{ #category : #accessing }
ClapCommandTest >> classToTest [
	^ ClapCommand
]

{ #category : #tests }
ClapCommandTest >> testAddFlag [
	| flag |
	flag := ClapFlag withName: 'flag'.
	subject add: flag.
	
	self assert: (subject flags includes: flag)
]

{ #category : #tests }
ClapCommandTest >> testAddSubcommand [
	| cmd |
	cmd := ClapCommand withName: 'cmd'.
	subject add: cmd.
	
	self assert: (subject subcommands includes: cmd)
]

{ #category : #tests }
ClapCommandTest >> testMatches [
	| arg argv match |
	arg := self subjectName copy.
	argv := { arg. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self assert: match word equals: arg.
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchesWithSingleFlag [
	| argv match |
	subject add: (ClapFlag withName: 'bar').
	argv := { self subjectName . '--bar' . #remainder } readStream.
		
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'bar').
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchesWithSinglePositional [
	| argv match |
	subject add: (ClapPositional withName: 'bar').
	argv := { self subjectName copy. 'valueforbar'. #remainder } readStream.

	match := subject matchOn: argv.

	self deny: match isMismatch.
	self
		assert: match word
		equals: self subjectName.
	self assert: (match includesMatchNamed: 'bar').
	self
		assert: (match atName: 'bar') word
		equals: 'valueforbar'.
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchesWithSingleSubcommand [
	| argv match |
	subject add: (ClapCommand withName: 'bar').
	argv := { self subjectName. 'bar'. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self assert: (match includesMatchNamed: 'bar').
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchingStopsAtWrongFlag [
	| argv badFlag match |
	subject add: (ClapFlag withName: 'bar').
	badFlag := '--notbar'.
	argv := { self subjectName. badFlag } readStream.

	match := subject matchOn: argv.

	self assert: match isMatch.
	self
		assert: argv next
		identicalTo: badFlag
]

{ #category : #tests }
ClapCommandTest >> testMismatchesDifferentAlias [
	| argv badCommand match |
	badCommand := self subjectName , 'NOT'.
	argv := { badCommand } readStream.
	
	match := subject matchOn: argv.
	
	self assert: match isMismatch.
	self
		assert: argv next
		identicalTo: badCommand
]
