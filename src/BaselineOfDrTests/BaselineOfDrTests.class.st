Class {
	#name : #BaselineOfDrTests,
	#superclass : #BaselineOf,
	#category : #BaselineOfDrTests
}

{ #category : #baselines }
BaselineOfDrTests >> baseline: spec [
	<baseline>
	spec
		for: #common
		do: [ spec
				package: 'DrTests';
				package: 'DrTests-TestsRunner' with: [ spec requires: #('DrTests') ];
				package: 'DrTests-TestCoverage' with: [ spec requires: #('DrTests') ];
				package: 'DrTests-TestsProfiling' with: [ spec requires: #('DrTests') ];
				package: 'DrTests-Tests' with: [ spec requires: #('DrTests' 'DrTests-TestsRunner' 'DrTests-TestCoverage-MocksForTests') ];
				package: 'DrTests-TestCoverage-Tests' with: [ spec requires: #('DrTests-TestCoverage' 'DrTests-TestCoverage-MocksForTests') ];
				package: 'DrTests-TestsProfiling-Tests' with: [ spec requires: #('DrTests-TestsProfiling') ];
				package: 'DrTests-TestCoverage-MocksForTests' with: [ spec requires: #('DrTests-TestCoverage') ];
				package: 'DrTests-CommentsToTests' with: [ spec requires: #('DrTests' 'DrTests-TestsRunner') ];
				package: 'DrTests-CommentsToTests-Tests' with: [ spec requires: #('DrTests-CommentsToTests') ] ]
]
