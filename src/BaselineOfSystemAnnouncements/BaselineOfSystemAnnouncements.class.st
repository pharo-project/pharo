Class {
	#name : 'BaselineOfSystemAnnouncements',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfSystemAnnouncements',
	#package : 'BaselineOfSystemAnnouncements'
}

{ #category : 'baselines' }
BaselineOfSystemAnnouncements >> baseline: spec [

	<baseline>
	
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
			spec
				baseline: 'Announcements' with: [ 
					spec
						className: 'BaselineOfAnnouncements';
						repository: repository;
						loads: 'Core'];
				package: 'System-Announcements' with: [spec requires: #('Announcements')].

			spec
				group: 'Core' with: #('System-Announcements');
            group: 'default' with: #('Core')]
]
