Extension { #name : #TestCase }

{ #category : #'*NautilusCommon' }
TestCase class >> clearResultOfTestMethod: aMethod [
 	#(passed failures errors) do: [ :each |
		(self lastRun at: each)
			remove: aMethod selector
			ifAbsent: [  ] ]
]

{ #category : #'*NautilusCommon' }
TestCase class >> correspondingClassTested [
	"Returns the class tested by this TestCase or nil if it can't be found."

	| classNameString |
	classNameString := self name asString.
	^ self environment
		at:
			((classNameString endsWith: 'Test')
				ifTrue: [ classNameString allButLast: 4	"'Test' size" ]
				ifFalse: [ classNameString ]) asSymbol
		ifAbsent: [ nil ]
]

{ #category : #'*NautilusCommon' }
TestCase class >> correspondingTestCase [
	^ nil
]

{ #category : #'*NautilusCommon' }
TestCase class >> hasErrorTest [

	^ (self history at: #errors) isEmpty not 
]

{ #category : #'*NautilusCommon' }
TestCase class >> hasFailedTest [

	^ (self history at: #failures) isEmpty not 
]

{ #category : #'*NautilusCommon' }
TestCase class >> hasPassedTest [

	^ (self history at: #passed) isEmpty not 
]
