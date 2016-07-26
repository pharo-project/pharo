versions
version415: spec
	<version: '4.15' imports: #('3.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Glamour version 4.15'.
		spec author: 'AndreiChis'.
		spec timestamp: '5/31/2016 15:02'.
		spec 
			project: 'Rubric' with: '2.19.3';
			project: 'FastTable' with: '0.10.2'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-TudorGirba.40';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-AndreiChis.59';
			package: 'Glamour-Core' with: 'Glamour-Core-TudorGirba.322';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.186';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-AndreiChis.112';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.303';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AndreiChis.421';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-TheIntegrator.14';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-CyrilFerlciot.112';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.5';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-HenrikNergaard.222';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.195';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-AndreiChis.349';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-GuillermoPolito.110';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-TudorGirba.137';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-TheIntegrator.54';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.35'. ].
