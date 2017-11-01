Extension { #name : #ReMultipleCritiquesEvent }

{ #category : #'*qualityAssistantRecording' }
ReMultipleCritiquesEvent >> dataForQAR [

	| data |

	data := super dataForQAR
		at: #critiques put:
			(critiques collect: #dataForQAREntityless as: Array);
		yourself.
		
	critiques ifEmpty: [ 
		(data at: #entity) removeKey: #sourceCode ifAbsent: [  ] ].

	^ data
]
