Class {
	#name : #BaselineOfEpicea,
	#superclass : #BaselineOf,
	#category : #BaselineOfEpicea
}

{ #category : #baselines }
BaselineOfEpicea >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'Ombu';
			package: 'Epicea';
			package: 'Hiedra';
			package: 'Hiedra-Examples';
			package: 'EpiceaBrowsers'. ]
]
