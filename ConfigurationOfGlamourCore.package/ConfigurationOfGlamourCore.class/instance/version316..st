versions
version316: spec
	<version: '3.1.6' imports: #('3.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.1.6'.
		spec author: 'FranckWarlouzet'.
		spec timestamp: '6/29/2015 16:34'.
		spec project: 'Rubric' with: '1.10'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.39';
			package: 'Glamour-Core' with: 'Glamour-Core-EstebanLorenzano.300';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.177';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-TorstenBergmann.109';
			package: 'Glamour-Examples' with: 'Glamour-Examples-CyrilFerlicot.292';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-StefanReichhart.398';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-AliakseiSyrel.13';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AliakseiSyrel.104';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-MarcusDenker.214';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-FranckWarlouzet.178';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-CyrilFerlicot.322';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AndreiChis.103';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.124';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-CyrilFerlicot.43';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. ].
