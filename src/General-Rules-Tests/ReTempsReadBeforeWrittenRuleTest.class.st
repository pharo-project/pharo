Class {
	#name : 'ReTempsReadBeforeWrittenRuleTest',
	#superclass : 'ReAbstractRuleTestCase',
	#category : 'General-Rules-Tests-Potential Bugs',
	#package : 'General-Rules-Tests',
	#tag : 'Potential Bugs'
}

{ #category : 'helpers' }
ReTempsReadBeforeWrittenRuleTest >> mockMethod [
	| temp1 temp2 temp3 |
	temp3 := 1.
	[ temp2 := 2 ] value.
	^ { temp1 . temp2 . temp3 }
]

{ #category : 'tests' }
ReTempsReadBeforeWrittenRuleTest >> testFoundTemporaries [

	| temporaries |
	temporaries := self subjectUnderTest new
		               findReadsBeforeWrittenTemporariesIn:
		               self class >> #mockMethod.

	self assertCollection: temporaries hasSameElements: #( temp1 temp2 )
]
