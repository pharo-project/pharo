Class {
	#name : #ClapPositionalTest,
	#superclass : #ClapParameterTest,
	#category : #'Clap-Tests-Unit'
}

{ #category : #accessing }
ClapPositionalTest >> classToTest [
	^ ClapPositional
]

{ #category : #tests }
ClapPositionalTest >> testMatches [
	| argv match |
	argv := { 'bar'. #remainder } readStream.
	
	match := subject matchOn: argv.
	
	self deny: match isMismatch.
	self
		assert: match specification
		identicalTo: subject.
	self assert: match word equals: 'bar'.
	self assert: argv next equals: #remainder
]
