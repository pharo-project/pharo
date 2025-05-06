Class {
	#name : 'ClapCommandSpecTest',
	#superclass : 'ClapParameterizedTest',
	#category : 'Clap-Tests-Unit',
	#package : 'Clap-Tests',
	#tag : 'Unit'
}

{ #category : 'accessing' }
ClapCommandSpecTest >> classToTest [
	^ ClapCommandSpec
]

{ #category : 'tests' }
ClapCommandSpecTest >> testAddFlag [
	| subject flag |
	subject := self namedSubject.
	flag := ClapFlag id: #flag.
	subject add: flag.
	
	self assert: (subject flags includes: flag)
]

{ #category : 'tests' }
ClapCommandSpecTest >> testAddSubcommand [
	| subject cmd |
	subject := self namedSubject.
	cmd := ClapCommandSpec id: #cmd.
	subject add: cmd.
	
	self assert: (subject subcommands includes: cmd)
]

{ #category : 'tests' }
ClapCommandSpecTest >> testMatches [
	| subject arg argv ctx match |
	subject := self namedSubject.
	arg := self subjectName copy.
	argv := { arg. #remainder } readStream.
	ctx := ClapContext new.
	
	match := subject matchOn: argv in: ctx.
	
	self deny: match isMismatch.
	self assert: match specification identicalTo: subject.
	self assert: match parent identicalTo: ctx.
	self assert: match word equals: arg.
	self assert: argv next equals: #remainder
]

{ #category : 'tests' }
ClapCommandSpecTest >> testMatchesWithSingleFlag [
	| subject argv args match |
	subject := self namedSubject.
	subject add: (ClapFlag id: #bar).
	args := { self subjectName . '--bar' . #remainder }.
	argv := args readStream.
		
	match := subject matchOn: argv in: (subject activationWith: args).
	
	self deny: match isMismatch.
	self assert: (match includesMatchOf: #bar).
	self assert: argv next equals: #remainder
]

{ #category : 'tests' }
ClapCommandSpecTest >> testMatchesWithSinglePositional [
	| subject args argv match |
	subject := self namedSubject.
	subject add: (ClapPositional id: #bar).
	args := { self subjectName copy. 'valueforbar'. #remainder }.
	argv := args readStream.

	match := subject matchOn: argv in: (subject activationWith: args).

	self deny: match isMismatch.
	self assert: match word equals: self subjectName.
	self assert: (match includesMatchOf: #bar).
	self
		assert: (match at: #bar) word
		equals: 'valueforbar'.
	self assert: argv next equals: #remainder
]

{ #category : 'tests' }
ClapCommandSpecTest >> testMatchesWithSingleSubcommand [
	| subject args argv match |
	subject := self namedSubject.
	subject add: (ClapCommandSpec id: #bar).
	args := { self subjectName. 'bar'. #remainder }.
	argv := args readStream.
	
	match := subject matchOn: argv in: (subject activationWith: args).
	
	self deny: match isMismatch.
	self assert: (match includesMatchOf: #bar).
	self assert: argv next equals: #remainder
]

{ #category : 'tests' }
ClapCommandSpecTest >> testMatchingStopsAtWrongFlag [
	| subject args argv badFlag match |
	subject := self namedSubject.
	subject add: (ClapFlag id: #bar).
	badFlag := '--notbar'.
	args := { self subjectName. badFlag }.
	argv := args readStream.

	match := subject matchOn: argv in: (subject activationWith: args).

	self assert: match isMatch.
	self assert: argv next identicalTo: badFlag
]

{ #category : 'tests' }
ClapCommandSpecTest >> testMismatchesDifferentAlias [
	| subject argv badCommand match |
	subject := self namedSubject.
	badCommand := self subjectName , 'NOT'.
	argv := { badCommand } readStream.
	
	match := subject matchOn: argv in: Object new.
	
	self assert: match isMismatch.
	self assert: argv next identicalTo: badCommand
]
