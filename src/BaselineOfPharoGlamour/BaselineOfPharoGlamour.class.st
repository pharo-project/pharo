Class {
	#name : #BaselineOfPharoGlamour,
	#superclass : #BaselineOf,
	#category : #BaselineOfPharoGlamour
}

{ #category : #baselines }
BaselineOfPharoGlamour >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'Glamour-Announcements';
			package: 'Glamour-Helpers';
			package: 'Glamour-Core';
			package: 'Glamour-Tasks';
			package: 'Glamour-PagerModel';
			package: 'Glamour-Presentations';
			package: 'Glamour-Browsers';
			package: 'Glamour-Morphic-Brick';
			package: 'Glamour-Morphic-Theme';
			package: 'Glamour-Morphic-Widgets';
			package: 'Glamour-Morphic-Renderer';
			package: 'Glamour-Morphic-Pager';
			package: 'Glamour-Rubric-Presentations';
			package: 'Glamour-FastTable';
			package: 'Glamour-SpecIntegration';
			package: 'Glamour-Tests-Core';
			package: 'Glamour-Tests-Resources';
			package: 'Glamour-Tests-Morphic';
			package: 'Glamour-Morphic-Brick-Tests';
			package: 'Glamour-Tests-Rubric'.
		spec 
			group: 'Core' with: #('Glamour-Announcements'	'Glamour-Helpers'	'Glamour-Core'
				'Glamour-Tasks'	'Glamour-PagerModel'	'Glamour-Presentations'	'Glamour-Browsers'
				'Glamour-Morphic-Brick'	'Glamour-Morphic-Theme'	'Glamour-Morphic-Widgets'
				'Glamour-Morphic-Renderer'	'Glamour-Morphic-Pager'	'Glamour-Rubric-Presentations'
				'Glamour-FastTable' 'Glamour-SpecIntegration');
			group: 'Tests' with: #('Glamour-Tests-Core' 'Glamour-Tests-Resources' 'Glamour-Tests-Morphic'
				'Glamour-Morphic-Brick-Tests' 'Glamour-Tests-Rubric');
			group: 'default' with: #('Core' 'Tests') ]
]
