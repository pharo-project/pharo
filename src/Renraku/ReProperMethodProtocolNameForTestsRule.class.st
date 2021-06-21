"
Rule to favour 'tests' method protocol for SUnit tests over 'test' as typically several test methods can/will be in the category
"
Class {
	#name : #ReProperMethodProtocolNameForTestsRule,
	#superclass : #ReProperMethodProtocolNameRule,
	#category : #'Renraku-Rules'
}

{ #category : #running }
ReProperMethodProtocolNameForTestsRule >> basicCheck: aMethod [
	"We only check for SUnit test methods"
	
	^ aMethod isTestMethod and: [ self badMethodProtocolNames includes: aMethod category ]
]

{ #category : #idioms }
ReProperMethodProtocolNameForTestsRule >> protocolIdiom [ 

	^self use: 'tests' insteadOf: #('test')
]
