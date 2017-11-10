Class {
	#name : #RBLintRuleTest,
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
	#category : #Refactoring-Tests-Core-Data
}

{ #category : #foo }
RBLintRuleTest class >> someFooMethod [
	^ 'does nothing here:)'
]

{ #category : #foo }
RBLintRuleTest class >> someOtherFooMethod [
	^ 'does nothing here even better:)'
]

{ #category : #accessing }
RBLintRuleTest >> checkClass: aSmalllintContext [
]

{ #category : #accessing }
RBLintRuleTest >> checkMethod: aSmalllintContext [
]

{ #category : #accessing }
RBLintRuleTest >> displayName [
	| nameStream |
	nameStream := WriteStream on: (String new: 64).
	nameStream nextPutAll: self name;
		nextPutAll: ' ('.
	self problemCount printOn: nameStream.
	nameStream nextPut: $).
	^nameStream contents
]

{ #category : #private }
RBLintRuleTest >> failedRules [
	^self isEmpty
		ifTrue: [#()]
		ifFalse: [Array with: self]
]

{ #category : #testing }
RBLintRuleTest >> hasConflicts [
	^false
]

{ #category : #initialization }
RBLintRuleTest >> initialize [
	name := ''
]

{ #category : #testing }
RBLintRuleTest >> isComposite [
	^false
]

{ #category : #testing }
RBLintRuleTest >> isEmpty [
	self subclassResponsibility
]

{ #category : #testing }
RBLintRuleTest >> junk [
	^ RBRefactoryTestDataApp printString 
		copyFrom: 1
		to: CR
]

{ #category : #accessing }
RBLintRuleTest >> name [
	^name
]

{ #category : #accessing }
RBLintRuleTest >> name: aString [ 
	name := aString
]

{ #category : #accessing }
RBLintRuleTest >> openEditor [
	| rules |
	rules := self failedRules.
	rules isEmpty ifTrue: [^self].
	rules size == 1 ifTrue: [^rules first viewResults]
]

{ #category : #printing }
RBLintRuleTest >> printOn: aStream [ 
	name isNil
		ifTrue: [super printOn: aStream]
		ifFalse: [aStream nextPutAll: name]
]

{ #category : #accessing }
RBLintRuleTest >> problemCount [
	^self subclassResponsibility
]

{ #category : #initialize-release }
RBLintRuleTest >> resetResult [
]

{ #category : #accessing }
RBLintRuleTest >> run [
	^Object printOn: self
]

{ #category : #accessing }
RBLintRuleTest >> runOnEnvironment: anEnvironment [ 
	^Object printOn: self onEnvironment: anEnvironment
]

{ #category : #accessing }
RBLintRuleTest >> someOtherDemoMethod [
	| temp |
	temp := self new.
	^ temp junk
]

{ #category : #private }
RBLintRuleTest >> viewResults [
	self subclassResponsibility
]
