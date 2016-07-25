versions
version47: spec
	<version: '4.7' imports: #('3.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Fix for 1issue 7837'.
		spec author: 'AndreiChis'.
		spec timestamp: '3/17/2016 15:08'.
		spec 
			project: 'Rubric' with: '2.11';
			project: 'FastTable' with: '0.10.1'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-TudorGirba.40';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-AndreiChis.59';
			package: 'Glamour-Core' with: 'Glamour-Core-GuillermoPolito.312';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-TudorGirba.182';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-AndreiChis.111';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.303';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AliakseiSyrel.412';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-TheIntegrator.14';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AndreiChis.111';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.5';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-HenrikNergaard.222';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AliakseiSyrel.192';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-TudorGirba.346';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AliakseiSyrel.107';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-TudorGirba.137';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-TudorGirba.51';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-TudorGirba.21'. ].
