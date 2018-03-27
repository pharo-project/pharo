"
This filter only accepts code changes that are latest, i.e. that aren't overridden by other changes in the log more recently.
"
Class {
	#name : #EpLatestCodeChangeFilter,
	#superclass : #EpFilter,
	#instVars : [
		'logBrowserModel'
	],
	#category : #'EpiceaBrowsers-Filters'
}

{ #category : #accessing }
EpLatestCodeChangeFilter class >> title [

	^ 'Show only latest changes'
]

{ #category : #comparing }
EpLatestCodeChangeFilter >> = other [

	^ self class = other class
]

{ #category : #filtering }
EpLatestCodeChangeFilter >> accepts: anOmEntry [

	| finalReference log |
	log := logBrowserModel log.
	finalReference := log referenceTo: anOmEntry.
	anOmEntry content isCodeChange ifFalse: [ ^ false ].

	logBrowserModel cachedLogEntries reverseDo: [:each |
		(log referenceTo: each) = finalReference ifTrue: [ ^ true ].
		(each content isCodeChange and: [
			each content doesOverride: anOmEntry content ]) ifTrue: [ ^ false ].
	].

	^ false "shouldn't happen"
]

{ #category : #comparing }
EpLatestCodeChangeFilter >> hash [

	^ self class hash
]

{ #category : #accessing }
EpLatestCodeChangeFilter >> logBrowserModel [
	^ logBrowserModel
]

{ #category : #accessing }
EpLatestCodeChangeFilter >> logBrowserModel: anEpLogBrowserModel [ 
	
	logBrowserModel := anEpLogBrowserModel
]

{ #category : #accessing }
EpLatestCodeChangeFilter >> title [

	^ self class title
]
