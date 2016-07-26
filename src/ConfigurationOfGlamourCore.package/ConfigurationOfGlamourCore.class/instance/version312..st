versions
version312: spec
	<version: '3.1.2' imports: #('3.1-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.1.2'.
		spec author: 'AndreiChis'.
		spec timestamp: '04/29/2015 11:07'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-TorstenBergmann.109';
			package: 'Glamour-Core' with: 'Glamour-Core-StefanReichhart.299';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TorstenBergmann.291';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.39';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-StefanReichhart.398';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-AliakseiSyrel.13';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AndreiChis.103';
			package: 'Glamour-Morphic-Renderer' with:'Glamour-Morphic-Renderer-AndreiChis.321';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-StefanReichhart.212';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.177';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.177';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.41';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AliakseiSyrel.104';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.124';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14'. 
		
		spec 
			project: 'Rubric' with: '1.2.12' ].

