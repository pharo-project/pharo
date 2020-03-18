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
	| subject flag |
	subject := self namedSubject.
	flag := ClapFlag id: #flag.
	subject add: flag.
	
	self assert: (subject flags includes: flag)
]

{ #category : #tests }
ClapCommandTest >> testAddSubcommand [
	| subject cmd |
	subject := self namedSubject.
	cmd := ClapCommand id: #cmd.
	subject add: cmd.
	
	self assert: (subject subcommands includes: cmd)
]

{ #category : #tests }
ClapCommandTest >> testMatches [
	| subject arg argv match |
	subject := self namedSubject.
	arg := self subjectName copy.
	argv := { arg. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self assert: match word equals: arg.
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchesWithSingleFlag [
	| subject argv match |
	subject := self namedSubject.
	subject add: (ClapFlag id: #bar).
	argv := { self subjectName . '--bar' . #remainder } readStream.
		
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self assert: (match includesMatchOf: #bar).
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchesWithSinglePositional [
	| subject argv match |
	subject := self namedSubject.
	subject add: (ClapPositional id: #bar).
	argv := { self subjectName copy. 'valueforbar'. #remainder } readStream.

	match := subject matchOn: argv.

	self deny: match isMismatch.
	self
		assert: match word
		equals: self subjectName.
	self assert: (match includesMatchOf: #bar).
	self
		assert: (match at: #bar) word
		equals: 'valueforbar'.
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchesWithSingleSubcommand [
	| subject argv match |
	subject := self namedSubject.
	subject add: (ClapCommand id: #bar).
	argv := { self subjectName. 'bar'. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self assert: (match includesMatchOf: #bar).
	self assert: argv next equals: #remainder
]

{ #category : #tests }
ClapCommandTest >> testMatchingStopsAtWrongFlag [
	| subject argv badFlag match |
	subject := self namedSubject.
	subject add: (ClapFlag id: #bar).
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
	| subject argv badCommand match |
	subject := self namedSubject.
	badCommand := self subjectName , 'NOT'.
	argv := { badCommand } readStream.
	
	match := subject matchOn: argv.
	
	self assert: match isMismatch.
	self
		assert: argv next
		identicalTo: badCommand
]
