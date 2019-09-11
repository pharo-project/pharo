Class {
	#name : #RBDummyLintRuleTest,
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
	#category : #'Refactoring2-Transformations-Tests'
}

{ #category : #foo }
RBDummyLintRuleTest class >> someFooMethod [
	^ 'does nothing here:)'
]

{ #category : #foo }
RBDummyLintRuleTest class >> someOtherFooMethod [
	^ 'does nothing here even better:)'
]

{ #category : #accessing }
RBDummyLintRuleTest >> checkClass: aSmalllintContext [
]

{ #category : #accessing }
RBDummyLintRuleTest >> checkMethod: aSmalllintContext [
]

{ #category : #accessing }
RBDummyLintRuleTest >> displayName [
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	nameStream nextPutAll: self name;
		nextPutAll: ' ('.
	self problemCount printOn: nameStream.
	nameStream nextPut: $).
	^nameStream contents
]

{ #category : #private }
RBDummyLintRuleTest >> failedRules [
	^self isEmpty
		ifTrue: [#()]
		ifFalse: [Array with: self]
]

{ #category : #testing }
RBDummyLintRuleTest >> hasConflicts [
	^false
]

{ #category : #initialization }
RBDummyLintRuleTest >> initialize [
	name := ''
]

{ #category : #testing }
RBDummyLintRuleTest >> isComposite [
	^false
]

{ #category : #testing }
RBDummyLintRuleTest >> isEmpty [
	self subclassResponsibility
]

{ #category : #testing }
RBDummyLintRuleTest >> junk [
	^ RBDummyRefactoryTestDataApp printString 
		copyFrom: 1
		to: CR
]

{ #category : #accessing }
RBDummyLintRuleTest >> name [
	^name
]

{ #category : #accessing }
RBDummyLintRuleTest >> name: aString [ 
	name := aString
]

{ #category : #accessing }
RBDummyLintRuleTest >> openEditor [
	| rules |
	rules := self failedRules.
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]
]

{ #category : #printing }
RBDummyLintRuleTest >> printOn: aStream [

	name ifNil: [ super printOn: aStream ] ifNotNil: [ aStream nextPutAll: name ]
]

{ #category : #accessing }
RBDummyLintRuleTest >> problemCount [
	^self subclassResponsibility
]

{ #category : #initialization }
RBDummyLintRuleTest >> resetResult [
]

{ #category : #accessing }
RBDummyLintRuleTest >> run [
	^Object printOn: self
]

{ #category : #accessing }
RBDummyLintRuleTest >> someOtherDemoMethod [
	| temp |
	temp := self new.
	^ temp junk
]

{ #category : #private }
RBDummyLintRuleTest >> viewResults [
	self subclassResponsibility
]
