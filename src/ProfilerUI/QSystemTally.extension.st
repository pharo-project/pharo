Extension { #name : 'QSystemTally' }

{ #category : '*ProfilerUI' }
QSystemTally >> <= aQSystemTally [
	^ tally <= aQSystemTally tally
]

{ #category : '*ProfilerUI' }
QSystemTally >> asTallyModel [
	^ TallyModelAndreas new
		item: self;
		yourself
]

{ #category : '*ProfilerUI' }
QSystemTally >> methodClassName [
	^ class ifNil: [ 'nil' ] ifNotNil: [ class name ]
]

{ #category : '*ProfilerUI' }
QSystemTally >> methodOrBlock [
	^ method
]

{ #category : '*ProfilerUI' }
QSystemTally >> originMethod [
	| methodOrBlock |
	methodOrBlock := self methodOrBlock.
	^ methodOrBlock isCompiledBlock
		  ifTrue: [ methodOrBlock method ]
		  ifFalse: [ methodOrBlock ]
]
