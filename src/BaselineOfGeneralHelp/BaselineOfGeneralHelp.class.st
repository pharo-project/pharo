Class {
	#name : #BaselineOfGeneralHelp,
	#superclass : #BaselineOf,
	#category : #BaselineOfGeneralHelp
}

{ #category : #baselines }
BaselineOfGeneralHelp >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'Announcements-Help';
			package: 'Pharo-Help';
			package: 'Regex-Help' ]
]
