Class {
	#name : #BaselineOfZodiac,
	#superclass : #BaselineOf,
	#category : #BaselineOfZodiac
}

{ #category : #baselines }
BaselineOfZodiac >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'Zinc-Zodiac';
			package: 'Zodiac-Extra';
			package: 'Zinc-Tests'.

		spec 
			group: 'Core' with: #('Zinc-Zodiac' 'Zodiac-Extra');
			group: 'Tests' with: #('Zinc-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
