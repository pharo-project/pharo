versions
version305: spec
	<version: '3.0.5' imports: #('3.0-baseline' )>

	spec for: #'common' do: [
		spec blessing: #'stable'.
		spec description: 'version 3.0.5'.
		spec author: 'AndreiChis'.
		spec timestamp: '12/10/2014 13:38'.
		spec 
			package: 'Glamour-Announcements' with: 'Glamour-Announcements-AndreiChis.8';
			package: 'Glamour-Helpers' with: 'Glamour-Helpers-AndreiChis.36';
			package: 'Glamour-Core' with: 'Glamour-Core-BernardoContreras.286';
			package: 'Glamour-Presentations' with: 'Glamour-Presentations-AndreiChis.166';
			package: 'Glamour-Browsers' with: 'Glamour-Browsers-BernardoContreras.107';
			package: 'Glamour-Tests-Core' with: 'Glamour-Tests-Core-AndreiChis.99';
			package: 'Glamour-Morphic-Widgets' with: 'Glamour-Morphic-Widgets-AndreiChis.153';
			package: 'Glamour-Morphic-Renderer' with: [
				spec file: 'Glamour-Morphic-Renderer-AndreiChis.305'.
				spec repository: 'http://www.smalltalkhub.com/mc/PharoExtras/GTToolsIntegration/main' ];
			package: 'Glamour-Morphic-Brick' with: 'Glamour-Morphic-Brick-AliakseiSyrel.86';
			package: 'Glamour-Morphic-Pager' with: 'Glamour-Morphic-Pager-AliakseiSyrel.80';
			package: 'Glamour-Morphic-Theme' with: 'Glamour-Morphic-Theme-TudorGirba.194';
			package: 'Glamour-Tests-Morphic' with: 'Glamour-Tests-Morphic-AndreiChis.118';
			package: 'Glamour-Examples' with: 'Glamour-Examples-TudorGirba.290';
			package: 'Glamour-Rubric-Presentations' with: 'Glamour-Rubric-Presentations-TudorGirba.33';
			package: 'Glamour-Tests-Rubric' with: 'Glamour-Tests-Rubric-AndreiChis.14';
			package: 'Glamour-Tests-Resources' with: 'Glamour-Tests-Resources-AndreiChis.3'. 
		
		spec 
			project: 'Rubric' with: '1.2.5' ].

