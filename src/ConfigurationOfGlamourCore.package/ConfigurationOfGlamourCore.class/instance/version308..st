versions
version308: spec
	<version: '3.0.8' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.8'.
		spec author: 'AndreiChis'.
		spec timestamp: '01/07/2015 17:33'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-BernardoContreras.107';
			package: 'Glamour-Core' with: 'Glamour-Core-EstebanLorenzano.287';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.290';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.36';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AliakseiSyrel.92';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AliakseiSyrel.82';
			package: 'Glamour-Morphic-Renderer' with:'Glamour-Morphic-Renderer-AndreiChis.304';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-TudorGirba.195';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.154';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.166';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.35';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-EstebanLorenzano.100';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.119';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. 
		
		spec 
			project: 'Rubric' with: '1.2.7' ].

