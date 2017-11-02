Extension { #name : #Interval }

{ #category : #'*qualityAssistantRecording' }
Interval >> dataForQAR [

	^ Dictionary newFromPairs: { 
		#from . start .
		#to   . stop }
]
