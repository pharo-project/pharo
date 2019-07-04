Class {
	#name : #SpecDemoTest,
	#superclass : #SpecSmokeTestCase,
	#category : #'Spec-Tests-Examples'
}

{ #category : #running }
SpecDemoTest >> classToTest [
	^ SpecDemo
]

{ #category : #tests }
SpecDemoTest >> testSmokeTestForDemoPages [
	self timeLimit: 1 minute. 
	SpecDemoPage availablePages
		do: [ :demoPage | 
			[ self shouldnt: [ window := demoPage new openWithSpec ] raise: Error description: 'Broken demo page: ' , demoPage asString ]
				ensure: [ window ifNotNil: #close ] ]
]
