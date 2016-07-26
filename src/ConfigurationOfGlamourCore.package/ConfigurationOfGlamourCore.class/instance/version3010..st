versions
version3010: spec
	<version: '3.0.10' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.10'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/16/2015 19:07'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-BernardoContreras.107';
			package: 'Glamour-Core' with: 'Glamour-Core-AndreiChis.288';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.290';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.36';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AliakseiSyrel.118';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AndreiChis.83';
			package: 'Glamour-Morphic-Renderer' with:'Glamour-Morphic-Renderer-AndreiChis.306';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-TudorGirba.196';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.160';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.169';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.37';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AndreiChis.101';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.120';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. 
		
		spec 
			project: 'Rubric' with: '1.2.8' ].

