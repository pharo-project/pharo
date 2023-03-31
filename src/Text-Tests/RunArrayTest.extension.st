Extension { #name : #RunArrayTest }

{ #category : #'*Text-Tests' }
RunArrayTest >> testScanFromANSICompatibility [

	RunArray scanFrom: '()f1dNumber new;;' readStream.
	RunArray scanFrom: '()a1death;;' readStream.
	RunArray scanFrom: '()F1death;;' readStream
]
