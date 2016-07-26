versions
version43: spec
	<version: '4.3' imports: #('3.3-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'GTools version 3.13'.
		spec author: 'AndreiChis'.
		spec timestamp: '2/23/2016 17:56'.
		spec 
			project: 'Rubric' with: '2.7';
			project: 'FastTable' with: '0.10'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-TudorGirba.40';
			package: 'Glamour-FastTable' with: 'Glamour-FastTable-TudorGirba.56';
			package: 'Glamour-Core' with: 'Glamour-Core-GuillermoPolito.312';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-TudorGirba.182';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-AndreiChis.111';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.303';
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AliakseiSyrel.410';
			package: 'Glamour-Morphic-Brick-Tests' with: 'Glamour-Morphic-Brick-Tests-TheIntegrator.14';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AndreiChis.111';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.5';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-AndreiChis.221';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.191';
			package: 'Glamour-Morphic-Renderer' with: 'Glamour-Morphic-Renderer-AndreiChis.345';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-SergeStinckwich.106';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-TudorGirba.137';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-AndreiChis.50';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-TudorGirba.21'. ].
