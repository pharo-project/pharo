Class {
	#name : #MetacelloVersionDiffReport,
	#superclass : #Object,
	#instVars : [
		#additions,
		#configuration,
		#modifications,
		#removals,
		#from,
		#to
	],
	#category : #'Metacello-Core-Model'
}

{ #category : #accessing }
MetacelloVersionDiffReport >> additions [
	additions ifNil: [ additions := Dictionary new ].
	^ additions
]

{ #category : #accessing }
MetacelloVersionDiffReport >> additions: anObject [
	additions := anObject
]

{ #category : #accessing }
MetacelloVersionDiffReport >> configuration [
	^ configuration
]

{ #category : #accessing }
MetacelloVersionDiffReport >> configuration: anObject [
	configuration := anObject
]

{ #category : #accessing }
MetacelloVersionDiffReport >> from [
	^ from
]

{ #category : #accessing }
MetacelloVersionDiffReport >> from: anObject [
	from := anObject
]

{ #category : #accessing }
MetacelloVersionDiffReport >> modifications [
	modifications ifNil: [ modifications := Dictionary new ].
	^ modifications
]

{ #category : #accessing }
MetacelloVersionDiffReport >> modifications: anObject [
	modifications := anObject
]

{ #category : #printing }
MetacelloVersionDiffReport >> printOn: aStream [
	| printBlock |
	printBlock := [ :pkgName :ar | 
	aStream
		tab: 2;
		nextPutAll: pkgName asString;
		cr.
	aStream
		tab: 3;
		print: (ar at: 1) asString ;
		nextPutAll: ' to ';
		print: (ar at: 2) asString ;
		cr ].
	aStream
		nextPutAll: self configuration asString;
		space;
		print: from asString ;
		nextPutAll: ' to ';
		print: to asString ;
		cr.
	aStream
		tab;
		nextPutAll: 'Additions:';
		cr.
	self additions keysAndValuesDo: printBlock.
	aStream
		tab;
		nextPutAll: 'Modifications:';
		cr.
	self modifications keysAndValuesDo: printBlock.
	aStream
		tab;
		nextPutAll: 'Removals:';
		cr.
	self removals keysAndValuesDo: printBlock
]

{ #category : #accessing }
MetacelloVersionDiffReport >> removals [
	removals ifNil: [ removals := Dictionary new ].
	^ removals
]

{ #category : #accessing }
MetacelloVersionDiffReport >> removals: anObject [
	removals := anObject
]

{ #category : #accessing }
MetacelloVersionDiffReport >> to [
	^ to
]

{ #category : #accessing }
MetacelloVersionDiffReport >> to: anObject [
	to := anObject
]
