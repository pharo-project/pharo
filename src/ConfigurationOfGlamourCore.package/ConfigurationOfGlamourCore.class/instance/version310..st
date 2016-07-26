versions
version310: spec
	<version: '3.10' imports: #('3.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Bugfixes and improvements.'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/18/2016 13:23'.
		spec 
			project: 'Rubric' with: '2.5';
			project: 'FastTable' with: '0.8.3'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-TudorGirba.40';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-CyrilFerlicot.52';
			package: 'Glamour-Core' with: 'Glamour-Core-AndreiChis.309';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-TudorGirba.182';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-AndreiChis.111';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.303';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AndreiChis.409';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-TheIntegrator.14';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-CyrilFerlicot.110';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.5';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.221';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.188';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-AndreiChis.343';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AndreiChis.105';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-SkipLentz.133';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.50';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.16'. ].
