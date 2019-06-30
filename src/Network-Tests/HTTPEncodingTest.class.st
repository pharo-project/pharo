"
SUnit tests for HTTPEncoding
"
Class {
	#name : #HTTPEncodingTest,
	#superclass : #TestCase,
	#category : #'Network-Tests-Protocols'
}

{ #category : #tests }
HTTPEncodingTest >> testEncodeForHTTP [

	self assert: 'aa aa éé aa aa' urlEncoded equals: 'aa%20aa%20%C3%A9%C3%A9%20aa%20aa'
]
