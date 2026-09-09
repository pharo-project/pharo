Class {
	#name : 'BaselineOfAnnouncements',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfAnnouncements',
	#package : 'BaselineOfAnnouncements'
}

{ #category : 'baselines' }
BaselineOfAnnouncements >> baseline: spec [

	<baseline>
	spec for: #common do: [
			spec
				package: 'Announcements-Core';
				package: 'Announcements-Core-Tests' with: [ spec requires: #( 'Announcements-Core' ) ].

			spec
				group: 'Core' with: { 'Announcements-Core' };
				group: 'Tests' with: { 'Announcements-Core-Tests' } ]
]
