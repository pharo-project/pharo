Class {
	#name : 'BaselineOfZodiac',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfZodiac',
	#package : 'BaselineOfZodiac'
}

{ #category : 'baselines' }
BaselineOfZodiac >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'Zinc-Zodiac-Tests';
			package: 'Zodiac-Extra';
			package: 'Zodiac-Tests'.

		spec 
			group: 'Core' with: #('Zodiac-Extra');
			group: 'Tests' with: #('Zodiac-Tests' 'Zinc-Zodiac-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
