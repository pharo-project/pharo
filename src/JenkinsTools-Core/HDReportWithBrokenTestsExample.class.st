Class {
	#name : #HDReportWithBrokenTestsExample,
	#superclass : #TestCase,
	#category : #'JenkinsTools-Core'
}

{ #category : #tests }
HDReportWithBrokenTestsExample >> testForCIWhichPassButItIsExpectedFailure [
	<expectedFailure>

	self assert: true
	
]

{ #category : #tests }
HDReportWithBrokenTestsExample >> testForCIWithMainAndBackgroundErrors [
	[self error: 'backgroundErrorFromAnnouncerTest2'] fork. 

	self error: 'mainErrorFromTestWithBackgroundError'
]

{ #category : #tests }
HDReportWithBrokenTestsExample >> testForCIWithMainAndBackgroundFailures [
	[self assert: false description: 'backgroundFailureFromAnnouncerTest2'] fork. 

	self assert: false description: 'mainFailureFromTestWithBackgroundFailure'
]

{ #category : #tests }
HDReportWithBrokenTestsExample >> testForCIWithOnlyBackgroundError [
	[self error: 'backgroundErrorFromAnnouncerTest'] fork. 

	
]

{ #category : #tests }
HDReportWithBrokenTestsExample >> testForCIWithOnlyBackgroundFailure [
	[self assert: false description: 'backgroundFailureFromAnnouncerTest'] fork. 

	
]
