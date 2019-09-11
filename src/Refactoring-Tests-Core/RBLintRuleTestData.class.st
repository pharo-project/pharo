"
This hierarchy is code that is used as test data to test the refactoring engine. As such the code might make
no sense or have lots of easy possible improvements... but as it is used for testing, they should not be fixed.
"
Class {
	#name : #RBLintRuleTestData,
	#superclass : #Object,
	#instVars : [
		'name',
		'foo1'
	],
	#classVars : [
		'Foo1'
	],
	#pools : [
		'TextConstants'
	],
	#category : #'Refactoring-Tests-Core-Data'
}

{ #category : #foo }
RBLintRuleTestData class >> someFooMethod [
	^ 'does nothing here:)'
]

{ #category : #foo }
RBLintRuleTestData class >> someOtherFooMethod [
	^ 'does nothing here even better:)'
]

{ #category : #accessing }
RBLintRuleTestData >> checkClass: aSmalllintContext [
]

{ #category : #accessing }
RBLintRuleTestData >> checkMethod: aSmalllintContext [
]

{ #category : #accessing }
RBLintRuleTestData >> displayName [
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	nameStream nextPutAll: self name;
		nextPutAll: ' ('.
	self problemCount printOn: nameStream.
	nameStream nextPut: $).
	^nameStream contents
]

{ #category : #private }
RBLintRuleTestData >> failedRules [
	^self isEmpty
		ifTrue: [#()]
		ifFalse: [Array with: self]
]

{ #category : #testing }
RBLintRuleTestData >> hasConflicts [
	^false
]

{ #category : #initialization }
RBLintRuleTestData >> initialize [
	name := ''
]

{ #category : #testing }
RBLintRuleTestData >> isComposite [
	^false
]

{ #category : #testing }
RBLintRuleTestData >> isEmpty [
	self subclassResponsibility
]

{ #category : #testing }
RBLintRuleTestData >> junk [
	^ RBRefactoryTestDataApp printString 
		copyFrom: 1
		to: CR
]

{ #category : #accessing }
RBLintRuleTestData >> name [
	^name
]

{ #category : #accessing }
RBLintRuleTestData >> name: aString [ 
	name := aString
]

{ #category : #accessing }
RBLintRuleTestData >> openEditor [
	| rules |
	rules := self failedRules.
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]
]

{ #category : #printing }
RBLintRuleTestData >> printOn: aStream [

	name ifNil: [ super printOn: aStream ] ifNotNil: [ aStream nextPutAll: name ]
]

{ #category : #accessing }
RBLintRuleTestData >> problemCount [
	^self subclassResponsibility
]

{ #category : #initialization }
RBLintRuleTestData >> resetResult [
]

{ #category : #accessing }
RBLintRuleTestData >> run [
	^Object printOn: self
]

{ #category : #accessing }
RBLintRuleTestData >> runOnEnvironment: anEnvironment [ 
	^Object printOn: self onEnvironment: anEnvironment
]

{ #category : #accessing }
RBLintRuleTestData >> someOtherDemoMethod [
	| temp |
	temp := self new.
	^ temp junk
]

{ #category : #private }
RBLintRuleTestData >> viewResults [
	self subclassResponsibility
]
