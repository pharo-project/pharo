"
Rule to favour 'tests' method protocol for SUnit tests over 'test' as typically several test methods can/will be in the category
"
Class {
	#name : 'ReProperMethodProtocolNameForTestsRule',
	#superclass : 'ReProperMethodProtocolNameRule',
	#category : 'Renraku-Coding Idiom Violation',
	#package : 'Renraku',
	#tag : 'Coding Idiom Violation'
}

{ #category : 'idioms' }
ReProperMethodProtocolNameForTestsRule class >> protocolIdiom [

	^self use: 'tests' insteadOf: #('test')
]

{ #category : 'running' }
ReProperMethodProtocolNameForTestsRule >> basicCheck: aMethod [
	"We only check for SUnit test methods"

	^ aMethod isTestMethod and: [ self class badMethodProtocolNames includes: aMethod protocolName ]
]
