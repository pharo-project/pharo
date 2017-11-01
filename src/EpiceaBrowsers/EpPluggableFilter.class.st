"
My instances decide if an OmEntry is accepted applying a pluggable block closure.
"
Class {
	#name : #EpPluggableFilter,
	#superclass : #EpFilter,
	#instVars : [
		'condition',
		'title'
	],
	#category : 'EpiceaBrowsers-Filters'
}

{ #category : #'special filters' }
EpPluggableFilter class >> after: aDateAndTime [

	^ self
		title: 'after ', aDateAndTime printString
		condition: [ :e | (e tags at: EpLog timeKey) > aDateAndTime ]
]

{ #category : #accessing }
EpPluggableFilter class >> allFilterSelectors [
	^ self class allSelectorsInProtocol: #'well-known filters'
]

{ #category : #accessing }
EpPluggableFilter class >> allFilters [
	^ self allFilterSelectors collect: [ :selector | self perform: selector ]
]

{ #category : #'special filters' }
EpPluggableFilter class >> before: aDateAndTime [

	^ self
		title: 'before ', aDateAndTime printString
		condition: [ :e | (e tags at: EpLog timeKey) < aDateAndTime ]
]

{ #category : #'well-known filters' }
EpPluggableFilter class >> codeChangeFilter [
	^ self
		title: 'Show only code changes'
		condition: [ :e | e content isCodeChange ]
]

{ #category : #'special filters' }
EpPluggableFilter class >> isBehaviorNamed: aString [

	| instanceSideBehaviorName |
	instanceSideBehaviorName := aString asInstanceSideBehaviorName.

	^ self
		title: 'is change to ', instanceSideBehaviorName
		condition: [ :entry |
			| event |
			event := entry content.
			(event isEpBehaviorChange or: [ event isEpMethodChange ]) and: [ 
			event behaviorAffectedName asInstanceSideBehaviorName = instanceSideBehaviorName ] ]
]

{ #category : #'special filters' }
EpPluggableFilter class >> isMethod: aMethod [

	^ self
		title: 'is ', aMethod printString
		condition: [ :e | 
			e content isEpMethodChange and: [ 
			e content behaviorAffectedName = aMethod parentName and: [ 
			e content methodAffectedSelector = aMethod selector ] ] ]
]

{ #category : #'special filters' }
EpPluggableFilter class >> isPackageNamed: packageName [ 

	^ self
		title: 'belongs to package named ', packageName
		condition: [ :e | 
			e content isCodeChange and: [ 
			e content affectedPackageName = packageName ] ]
]

{ #category : #'well-known filters' }
EpPluggableFilter class >> logBrowserOperationFilter [
	^ self
		title: 'Show only apply, revert or comment events'
		condition: [ :e | e content isKindOf: EpLogBrowserOperation ]
]

{ #category : #'well-known filters' }
EpPluggableFilter class >> methodFilter [
	^ self
		title: 'Show only changes to methods'
		condition: [ :e | e content isEpMethodChange ]
]

{ #category : #'well-known filters' }
EpPluggableFilter class >> noTriggerFilter [
	^ self
		title: 'Hide non-manual changes (e.g. changes produced by Monticello)'
		condition: [ :e | 
			e content isCodeChange and: [
			(e tags includesKey: EpLog triggererReferenceKey) not ] ]

]

{ #category : #'instance creation' }
EpPluggableFilter class >> title: aString condition: aBlockClosure [ 
	^ self new
		title: aString;
		condition: aBlockClosure;
		yourself
]

{ #category : #'special filters' }
EpPluggableFilter class >> todayFilter [
	^ self
		title: 'today'
		condition: [ :e | DateAndTime today < (e tags at: EpLog timeKey ifAbsent: [ DateAndTime epoch ])  ]
]

{ #category : #comparing }
EpPluggableFilter >> = other [

	^ self class = other class and: [ self title = other title ]
]

{ #category : #filtering }
EpPluggableFilter >> accepts: anEntry [
	^ self condition value: anEntry
]

{ #category : #accessing }
EpPluggableFilter >> condition [
	^ condition
]

{ #category : #accessing }
EpPluggableFilter >> condition: aBlockClosure [ 
	condition := aBlockClosure 
]

{ #category : #comparing }
EpPluggableFilter >> hash [
	^ self class hash bitXor: self title hash 
]

{ #category : #printing }
EpPluggableFilter >> printOn: aStream [
	super printOn: aStream.
	aStream 
		nextPut: $(;
		nextPutAll: self title;
		nextPut: $)
			

]

{ #category : #accessing }
EpPluggableFilter >> title [
	^ title
]

{ #category : #accessing }
EpPluggableFilter >> title: aString [ 
	title := aString
]
