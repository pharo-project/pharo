versions
version341: spec
	<version: '3.4.1' imports: #('3.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'Changes in the use of #garbageCollect'.
		spec author: 'AndreiChis'.
		spec timestamp: '11/25/2015 18:00'.
		spec project: 'Rubric' with: '2.4'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.39';
			package: 'Glamour-Core' with: 'Glamour-Core-AndreiChis.306';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.179';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-TorstenBergmann.109';
			package: 'Glamour-Examples' with: 'Glamour-Examples-AndreiChis.301';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AndreiChis.408';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-AliakseiSyrel.13';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AndreiChis.109';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.5';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.221';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AliakseiSyrel.184';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-AliakseiSyrel.329';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-CyrilFerlicot.104';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.129';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.47';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.16'. ].
