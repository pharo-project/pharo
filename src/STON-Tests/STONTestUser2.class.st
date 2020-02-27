"
STONTestUser2 extends STONTestUser with explicit implementations of #fromSton: and #stonOn:
"
Class {
	#name : #STONTestUser2,
	#superclass : #STONTestUser,
	#category : #'STON-Tests-Support'
}

{ #category : #'ston-core' }
STONTestUser2 class >> stonName [
	^ #TestUser2
]

{ #category : #'ston-core' }
STONTestUser2 >> fromSton: stonReader [
	stonReader parseMapDo: [ :key :value |
		key = #username ifTrue: [ username := value ].
		key = #password ifTrue: [ password := value ].
		key = #enabled ifTrue: [ enabled := value ] ]
	
]

{ #category : #'ston-core' }
STONTestUser2 >> stonOn: stonWriter [	
	stonWriter writeObject: self streamMap: [ :dictionary |
		dictionary
			at: #username put: username;
			at: #password put: password;
			at: #enabled put: enabled ]
]
