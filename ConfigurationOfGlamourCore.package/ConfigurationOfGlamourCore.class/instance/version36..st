versions
version36: spec
	<version: '3.6' imports: #('3.2-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'This version fixes problems in GTDebugger and updates  Rubric (issues: 17155, 17279, 17152)'.
		spec author: 'AndreiChis'.
		spec timestamp: '1/8/2016 19:27'.
		spec project: 'Rubric' with: '2.5'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-TudorGirba.40';
			package: 'Glamour-Core' with: 'Glamour-Core-TudorGirba.307';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.179';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-AndreiChis.111';
			package: 'Glamour-Examples' with: 'Glamour-Examples-CyrilFerlicot.302';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AndreiChis.409';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-TheIntegrator.14';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-CyrilFerlicot.110';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.5';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.221';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.185';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-AndreiChis.333';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-CyrilFerlicot.104';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-SkipLentz.133';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.47';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.16'. ].
