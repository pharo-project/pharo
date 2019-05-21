Class {
	#name : #BaselineOfRecentActivity,
	#superclass : #BaselineOf,
	#category : #BaselineOfRecentActivity
}

{ #category : #baseline }
BaselineOfRecentActivity >> baseline: spec [
	<baseline>
	| repository | 
	repository := self packageRepositoryURL.
	
	spec
		for: #common
		do: [ 
			spec
			baseline: 'SUnitCore' with: [
				spec
					loads: 'Core';
					className: 'BaselineOfSUnit';
					repository: repository ].
			spec baseline: 'Spec' with: [spec repository: repository].
			
			spec package: 'RecentActivity' with: [ spec requires: 'Spec' ].
			spec package: 'RecentActivity-Tests' with: [ spec requires: #('RecentActivity' 'SUnitCore') ].

			"Groups"
			spec
				group: 'Core' with: #('RecentActivity');
				group: 'Tests' with: #('RecentActivity-Tests') ]
]
