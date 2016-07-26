versions
version325: spec
	<version: '3.2.5' imports: #('3.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'bugfixes'.
		spec author: 'AndreiChis'.
		spec timestamp: '10/15/2015 16:18'.
		spec project: 'Rubric' with: '2.1'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.39';
			package: 'Glamour-Core' with: 'Glamour-Core-SeanDeNigris.303';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.178';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-TorstenBergmann.109';
			package: 'Glamour-Examples' with: 'Glamour-Examples-AndreiChis.296';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-EstebanLorenzano.406';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-AliakseiSyrel.13';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-BernardoContreras.106';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.221';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.180';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-AndreiChis.327';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-CyrilFerlicot.104';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.124';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.44';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. ].
