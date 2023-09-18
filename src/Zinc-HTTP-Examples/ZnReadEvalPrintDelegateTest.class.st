"
Unit test for ZnReadEvalPrintDelegate
"
Class {
	#name : #ZnReadEvalPrintDelegateTest,
	#superclass : #TestCase,
	#category : #'Zinc-HTTP-Examples'
}

{ #category : #private }
ZnReadEvalPrintDelegateTest >> port [
	^ 1700 + 32 atRandom
]

{ #category : #testing }
ZnReadEvalPrintDelegateTest >> testReadEvalPrint [
	self withServerDo: [ :server | | x y result |
		server delegate: ZnReadEvalPrintDelegate new.
		x := 100 atRandom.
		y := 100 atRandom.
		result := ZnClient new
			url: server localUrl;
			addPathSegment: #repl;
			contents: ('{1}+{2}' format: { x. y });
			post.
		self assert: result asInteger equals: x + y ]
]

{ #category : #private }
ZnReadEvalPrintDelegateTest >> withServerDo: block [
	ZnServer withOSAssignedPortDo: block
]
