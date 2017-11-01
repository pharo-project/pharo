"
I am a private class used by AnnotationPanePlugin
"
Class {
	#name : #AnnotationRequest,
	#superclass : #Object,
	#instVars : [
		'separator',
		'annotationRequests',
		'method'
	],
	#category : 'NautilusCommon-Plugin'
}

{ #category : #instance-creation }
AnnotationRequest class >> onClass: aClass selector: aSelector [
	^ self new
		onClass: aClass selector: aSelector;
		yourself
]

{ #category : #requests }
AnnotationRequest >> allChangeSets [
	"A list of all change sets bearing the method."

	| changeSets |
	changeSets := ChangeSet allChangeSetsWithClass: self methodClass selector: self methodSelector.
	^ changeSets isEmpty
		ifFalse: [ 
			String
				streamContents: [ :aStream | 
					changeSets size = 1
						ifTrue: [ aStream nextPutAll: 'only in change set ' ]
						ifFalse: [ aStream nextPutAll: 'in change sets: ' ].
					changeSets
						do: [ :aChangeSet | 
							aStream
								nextPutAll: aChangeSet name;
								nextPutAll: ' ' ] ] ]
		ifTrue: [ 'in no change set' ]
]

{ #category : #accessing }
AnnotationRequest >> annotationRequests [
	
	^ annotationRequests ifNil: [ annotationRequests := self defaultAnnotationInfo ]
]

{ #category : #accessing }
AnnotationRequest >> annotationRequests: annotationArray [
	"check defaultAnnotationInfo to know what you can pass in the array"
	annotationRequests := annotationArray
]

{ #category : #accessing }
AnnotationRequest >> defaultAnnotationInfo [
	"Answer a list of symbol characterizing all the available kinds of annotations"

	^ #(#timeStamp #messageCategory #implementorsCount #allChangeSets)	"Each symbol represents the info type:
		#timeStamp	
			The time stamp of the last submission of the method.
		#firstComment 
			The first comment in the method, if any.
		#masterComment 
			The comment at the beginning of the supermost implementor 
			of the method if any.
		#documentation 
			Comment at beginning of the method or, if it has none, 
			comment at the beginning of a superclass's implementation of the method.
		#messageCategory 
			Which method category the method lies in.
		#sendersCount 
			A report of how many senders there of the message.
		#implementorsCount 
			A report of how many implementors there are of the message.
		#recentChangeSet	 
			The most recent change set bearing the method.
		#allChangeSets	 
			A list of all change sets bearing the method.
		#priorVersionsCount: 
			A report of how many previous versions there are of the method.
		#priorTimeStamp 
			The time stamp of the penultimate submission of the method, if any.
		"
]

{ #category : #actions }
AnnotationRequest >> getAnnotations [
	^ String streamContents: [ :aStream | 
		((self annotationRequests 
			collect: [ :request | self perform: request ]) 
			reject: [ :stringOrNil | stringOrNil isEmptyOrNil ]) 
				do: [ :each | aStream nextPutAll: each ]
				separatedBy: [ aStream nextPutAll: separator ] ]
]

{ #category : #requests }
AnnotationRequest >> implementorsCount [
	"A report of how many implementors there are of the message."

	| implementorsCount |
	implementorsCount := self methodSelector implementors size.
	^ implementorsCount = 1
		ifTrue: [ '1 implementor' ]
		ifFalse: [ implementorsCount printString , ' implementors' ]
]

{ #category : #requests }
AnnotationRequest >> messageCategory [
	"Which method category the method lies in."

	^ self methodClass organization categoryOfElement: self methodSelector
]

{ #category : #accessing }
AnnotationRequest >> method [
	^ method
]

{ #category : #requests }
AnnotationRequest >> methodClass [
	^ method methodClass
]

{ #category : #requests }
AnnotationRequest >> methodSelector [
	^ method selector
]

{ #category : #initialization }
AnnotationRequest >> onCompiledMethod: aCompiledMethod [
	method := aCompiledMethod.
	separator := ' - '
]

{ #category : #requests }
AnnotationRequest >> sendersCount [
	"A report of how many senders there of the message."

	| sendersCount |
	sendersCount := (self systemNavigation allCallsOn: self methodSelector) size.
	^ sendersCount = 1
		ifTrue: [ '1 sender' ]
		ifFalse: [ sendersCount printString , ' senders' ]
]

{ #category : #accessing }
AnnotationRequest >> separator: aString [
	separator := aString
]

{ #category : #requests }
AnnotationRequest >> timeStamp [
	"The time stamp of the last submission of the method."

	^ method ifNotNil: [ method timeStamp ]
]
