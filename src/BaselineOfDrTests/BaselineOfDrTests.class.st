Class {
	#name : 'BaselineOfDrTests',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfDrTests',
	#package : 'BaselineOfDrTests'
}

{ #category : 'baselines' }
BaselineOfDrTests >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ 
			spec
				package: 'DrTests';
				package: 'Coverage';
				package: 'DrTests-TestsRunner' with: [ spec requires: #('DrTests') ];
				package: 'DrTests-TestCoverage' with: [ spec requires: #('DrTests' 'Coverage') ];
				package: 'DrTests-TestsProfiling' with: [ spec requires: #('DrTests') ];
				package: 'DrTests-Tests' with: [ spec requires: #('DrTests' 'DrTests-TestsRunner' 'DrTests-TestCoverage-Tests-Mocks') ];
				package: 'DrTests-TestCoverage-Tests' with: [ spec requires: #('DrTests-TestCoverage' 'DrTests-TestCoverage-Tests-Mocks') ];
				package: 'DrTests-TestsProfiling-Tests' with: [ spec requires: #('DrTests-TestsProfiling') ];
				package: 'DrTests-TestCoverage-Tests-Mocks' with: [ spec requires: #('DrTests-TestCoverage') ];
				package: 'DrTests-CommentsToTests' with: [ spec requires: #('DrTests' 'DrTests-TestsRunner') ];
				package: 'DrTests-CommentsToTests-Tests' with: [ spec requires: #('DrTests-CommentsToTests') ].
			self rottenTestsFinder: spec.
			spec package: 'DrTests-RottenGreenTestsFinder' with: [ spec requires: #('rotten-tests-finder') ]. ]
]

{ #category : 'actions' }
BaselineOfDrTests >> postload: loader package: packageSpec [

	self tools register: DrTests as: #testRunner
]

{ #category : 'baselines' }
BaselineOfDrTests >> rottenTestsFinder: spec [
	
	spec 
		package: 'RottenTestsFinder';
		package: 'RottenTestsFinder-FakeTests' with: [ spec requires: #('RottenTestsFinder') ];
		package: 'RottenTestsFinder-Tests' with: [ spec requires: #('RottenTestsFinder') ];
		group: 'rotten-tests-finder' with: #('RottenTestsFinder' 'RottenTestsFinder-FakeTests' 'RottenTestsFinder-Tests').
		
]
