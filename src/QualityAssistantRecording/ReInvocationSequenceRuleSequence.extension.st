Extension { #name : #ReInvocationSequenceRuleSequence }

{ #category : #'*qualityAssistantRecording' }
ReInvocationSequenceRuleSequence >> dataForQAR [

	^ Dictionary newFromPairs: { 
		#before . before .
		#after  . after }
]
