"
STONTestMap is used to support unit tests.

I am a Dictionary but I don't print my elements during #printOn: to allow safe inspection of structures containing cycles that would otherwise lead to infinite loops.
"
Class {
	#name : #STONTestMap,
	#superclass : #Dictionary,
	#category : #'STON-Tests-Support'
}

{ #category : #'instance creation' }
STONTestMap class >> classTree [
	^ self classTree: Object
]

{ #category : #'instance creation' }
STONTestMap class >> classTree: topClass [
	| map |
	map := IdentityDictionary new.
	topClass withAllSubclasses do: [ :eachClass | | info |
		(info := self new)
			at: #name put: eachClass name asString;
			at: #comment put: eachClass comment;
			at: #isMeta put: eachClass isMeta;
			at: #methods put: eachClass selectors.		
		map at: eachClass put: info ].
	map keysAndValuesDo: [ :eachClass :eachInfo |
		eachClass == topClass
			ifFalse: [ eachInfo at: #superclass put: (map at: eachClass superclass) ].
		eachInfo at: #subclasses put: (eachClass subclasses collect: [ :subClass | map at: subClass ]) ].
	^ map at: topClass
]

{ #category : #'instance creation' }
STONTestMap class >> classTreeExtended [
	^ self classTreeExtended: Object
]

{ #category : #'instance creation' }
STONTestMap class >> classTreeExtended: topClass [
	| map |
	map := IdentityDictionary new.
	topClass withAllSubclasses do: [ :eachClass | | info methodsInfo |
		(info := self new)
			at: #name put: eachClass name asString;
			at: #comment put: eachClass comment;
			at: #isMeta put: eachClass isMeta;
			at: #methods put: (methodsInfo := self new).
		eachClass methods do: [ :eachMethod | | methodInfo |
			(methodInfo := self new)
				at: #name put: eachMethod selector;
				at: #numArgs put: eachMethod numArgs;
				at: #class put: info.
			methodsInfo at: eachMethod selector put: methodInfo ].
		map at: eachClass put: info ].
	map keysAndValuesDo: [ :eachClass :eachInfo |
		eachClass == topClass 
			ifFalse: [ eachInfo at: #superclass put: (map at: eachClass superclass) ].
		eachInfo at: #subclasses put: (eachClass subclasses collect: [ :subClass | map at: subClass ]) ].
	^ map at: topClass
]

{ #category : #'ston-core' }
STONTestMap class >> stonName [
	^ #TestMap
]

{ #category : #printing }
STONTestMap >> printElementsOn: stream [
	stream
		nextPut: $(;
		nextPut: $#;
		print: self size;
		nextPut: $)
]
